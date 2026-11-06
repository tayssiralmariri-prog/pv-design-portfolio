;;; ===========================================================
;;;  PV_HOUSE_9000.LSP - 9,000 kWh/yr case: 25 modules portrait, house width 9.40 m
;;;  Project : Residential PV (Nürnberg region) - PVsyst near shading
;;;  Command : PVHOUSE
;;;  Units   : meters (INSUNITS = 6)
;;;  Output  : layers HOUSE, OBSTACLES, PV_<N>_<L/P>
;;;            -> export each layer with STLOUT for Blender/PVsyst
;;; ===========================================================
;;;
;;;  Geometry convention (before final rotation):
;;;    X  = across the house (eave -> ridge on the south slope)
;;;    Y  = along the ridge (house length)
;;;    The PV slope faces -X. At the end everything is rotated
;;;    by ROT degrees about (0,0,0) so that the PV slope faces
;;;    azimuth -14 deg (14 deg east of south). World Y = North.
;;;
;;;  Roof-plane coordinates used for panels and obstacles:
;;;    u = distance along the ridge from the gable end at Y=0  [m]
;;;    v = distance along the slope, measured up from the eave [m]
;;; ===========================================================

;; ---------- helper: make/set layer ----------
(defun pvh:layer (nm col)
  (command "_.-LAYER" "_M" nm "_C" (itoa col) nm "")
)

;; ---------- helper: move a flat object onto the south roof ----------
;; Objects are first drawn flat in WCS with x = v (up the slope),
;; y = u (along the ridge), z = height above the roof surface.
;; This matrix tilts them by TILT and lifts them to the eave height,
;; so they lie exactly on the roof plane with the roof's slope.
(vl-load-com)
(defun pvh:toroof (e)
  (vla-TransformBy
    (vlax-ename->vla-object e)
    (vlax-tmatrix
      (list (list CS  0.0 (- SN) 0.0)
            (list 0.0 1.0 0.0    0.0)
            (list SN  0.0 CS     HE)
            (list 0.0 0.0 0.0    1.0))))
)

;; ---------- helper: rectangle overlap test ----------
;; r and each obstacle o = (u1 u2 v1 v2)
(defun pvh:hit (r obs clr / hit)
  (foreach o obs
    (if (and (< (nth 0 r) (+ (nth 1 o) clr))
             (> (nth 1 r) (- (nth 0 o) clr))
             (< (nth 2 r) (+ (nth 3 o) clr))
             (> (nth 3 r) (- (nth 2 o) clr)))
      (setq hit T)))
  hit
)

;; ===========================================================
;;  Geometry recorder + Collada (.dae) writer for PVsyst
;;  Every object is also stored as a triangle mesh in world
;;  coordinates (after the final rotation), so PVHOUSE can write
;;  a .dae file that PVsyst imports directly (no Blender/MeshLab).
;;  object = (name vertex-list triangle-list)
;;  triangle = (i j k "MATERIAL")
;; ===========================================================
(setq *PVH-MATS*
  '(("PN_MODULES" "0.05 0.10 0.40")   ; PV top surface
    ("PANEL_BODY" "0.55 0.55 0.55")   ; module frame / sides / back
    ("HOUSE"      "0.85 0.85 0.80")   ; walls + roof
    ("OBSTACLE"   "0.80 0.20 0.20"))) ; chimney, skylights

;; roof transform for a point (same as pvh:toroof)
(defun pvh:roofpt (p)
  (list (- (* CS (car p)) (* SN (caddr p)))
        (cadr p)
        (+ (* SN (car p)) (* CS (caddr p)) HE))
)

;; final rotation about Z by ROT degrees (same as the ROTATE command)
(defun pvh:rotpt (p / a)
  (setq a (* pi (/ ROT 180.0)))
  (list (- (* (car p) (cos a)) (* (cadr p) (sin a)))
        (+ (* (car p) (sin a)) (* (cadr p) (cos a)))
        (caddr p))
)

;; 8 corners of an axis-aligned box
(defun pvh:boxpts (x1 y1 z1 x2 y2 z2)
  (list (list x1 y1 z1) (list x2 y1 z1) (list x2 y2 z1) (list x1 y2 z1)
        (list x1 y1 z2) (list x2 y1 z2) (list x2 y2 z2) (list x1 y2 z2))
)

;; 12 outward triangles of a box; top (z2) gets mtop, rest mside
(defun pvh:boxtris (mtop mside)
  (list (list 0 2 1 mside) (list 0 3 2 mside)
        (list 4 5 6 mtop)  (list 4 6 7 mtop)
        (list 0 1 5 mside) (list 0 5 4 mside)
        (list 1 2 6 mside) (list 1 6 5 mside)
        (list 2 3 7 mside) (list 2 7 6 mside)
        (list 3 0 4 mside) (list 3 4 7 mside))
)

(defun pvh:n (x) (rtos x 2 4))

(defun pvh:normal (a b c / u v n l)
  (setq u (mapcar '- b a)
        v (mapcar '- c a)
        n (list (- (* (cadr u) (caddr v)) (* (caddr u) (cadr v)))
                (- (* (caddr u) (car v)) (* (car u) (caddr v)))
                (- (* (car u) (cadr v)) (* (cadr u) (car v))))
        l (sqrt (apply '+ (mapcar '* n n))))
  (if (> l 1e-12) (mapcar '(lambda (x) (/ x l)) n) '(0.0 0.0 1.0))
)

(defun pvh:matsused (ts / r)
  (foreach tr ts
    (if (not (member (cadddr tr) r)) (setq r (append r (list (cadddr tr))))))
  r
)

(defun pvh:xyz (p) (strcat (pvh:n (car p)) " " (pvh:n (cadr p)) " " (pvh:n (caddr p))))

(defun pvh:writedae (fname objs / fh o nm vs ts k sel tn n)
  (setq fh (open fname "w"))
  (write-line "<?xml version=\"1.0\" encoding=\"utf-8\"?>" fh)
  (write-line "<COLLADA xmlns=\"http://www.collada.org/2005/11/COLLADASchema\" version=\"1.4.1\">" fh)
  (write-line "<asset><contributor><authoring_tool>PV_HOUSE.LSP</authoring_tool></contributor><created>2026-11-02T00:00:00</created><modified>2026-11-02T00:00:00</modified><unit name=\"meter\" meter=\"1\"/><up_axis>Z_UP</up_axis></asset>" fh)
  ;; effects + materials
  (write-line "<library_effects>" fh)
  (foreach m *PVH-MATS*
    (write-line (strcat "<effect id=\"" (car m) "-effect\"><profile_COMMON><technique sid=\"common\"><lambert><diffuse><color sid=\"diffuse\">"
                        (cadr m) " 1</color></diffuse></lambert></technique></profile_COMMON></effect>") fh))
  (write-line "</library_effects>" fh)
  (write-line "<library_materials>" fh)
  (foreach m *PVH-MATS*
    (write-line (strcat "<material id=\"" (car m) "-material\" name=\"" (car m)
                        "\"><instance_effect url=\"#" (car m) "-effect\"/></material>") fh))
  (write-line "</library_materials>" fh)
  ;; geometries
  (write-line "<library_geometries>" fh)
  (foreach o objs
    (setq nm (car o) vs (cadr o) ts (caddr o))
    (write-line (strcat "<geometry id=\"" nm "-mesh\" name=\"" nm "\"><mesh>") fh)
    (write-line (strcat "<source id=\"" nm "-pos\"><float_array id=\"" nm "-pos-array\" count=\""
                        (itoa (* 3 (length vs))) "\">") fh)
    (foreach p vs (write-line (pvh:xyz p) fh))
    (write-line (strcat "</float_array><technique_common><accessor source=\"#" nm "-pos-array\" count=\""
                        (itoa (length vs)) "\" stride=\"3\"><param name=\"X\" type=\"float\"/><param name=\"Y\" type=\"float\"/><param name=\"Z\" type=\"float\"/></accessor></technique_common></source>") fh)
    (write-line (strcat "<source id=\"" nm "-nrm\"><float_array id=\"" nm "-nrm-array\" count=\""
                        (itoa (* 3 (length ts))) "\">") fh)
    (foreach tr ts
      (write-line (pvh:xyz (pvh:normal (nth (car tr) vs) (nth (cadr tr) vs) (nth (caddr tr) vs))) fh))
    (write-line (strcat "</float_array><technique_common><accessor source=\"#" nm "-nrm-array\" count=\""
                        (itoa (length ts)) "\" stride=\"3\"><param name=\"X\" type=\"float\"/><param name=\"Y\" type=\"float\"/><param name=\"Z\" type=\"float\"/></accessor></technique_common></source>") fh)
    (write-line (strcat "<vertices id=\"" nm "-vtx\"><input semantic=\"POSITION\" source=\"#" nm "-pos\"/></vertices>") fh)
    (foreach m (pvh:matsused ts)
      (setq k 0 sel nil)
      (foreach tr ts
        (if (= (cadddr tr) m) (setq sel (cons (cons k tr) sel)))
        (setq k (1+ k)))
      (write-line (strcat "<triangles material=\"" m "\" count=\"" (itoa (length sel))
                          "\"><input semantic=\"VERTEX\" source=\"#" nm "-vtx\" offset=\"0\"/><input semantic=\"NORMAL\" source=\"#"
                          nm "-nrm\" offset=\"1\"/><p>") fh)
      (foreach s (reverse sel)
        (setq tn (itoa (car s)))
        (write-line (strcat (itoa (cadr s)) " " tn " " (itoa (caddr s)) " " tn " "
                            (itoa (cadddr s)) " " tn) fh))
      (write-line "</p></triangles>" fh))
    (write-line "</mesh></geometry>" fh))
  (write-line "</library_geometries>" fh)
  ;; scene: one node per object
  (write-line "<library_visual_scenes><visual_scene id=\"Scene\" name=\"Scene\">" fh)
  (foreach o objs
    (setq nm (car o))
    (write-line (strcat "<node id=\"" nm "\" name=\"" nm "\" type=\"NODE\"><instance_geometry url=\"#"
                        nm "-mesh\" name=\"" nm "\"><bind_material><technique_common>") fh)
    (foreach m (pvh:matsused (caddr o))
      (write-line (strcat "<instance_material symbol=\"" m "\" target=\"#" m "-material\"/>") fh))
    (write-line "</technique_common></bind_material></instance_geometry></node>" fh))
  (write-line "</visual_scene></library_visual_scenes>" fh)
  (write-line "<scene><instance_visual_scene url=\"#Scene\"/></scene>" fh)
  (write-line "</COLLADA>" fh)
  (close fh)
)

;; ===========================================================
(defun c:PVHOUSE ( / L W H_RIDGE HE TILT PA PB PT STANDOFF GAP
                     M_END M_EAVE M_RIDGE CLR ROT SKY CHIM CHIM_H
                     TR CS SN RISE S N ORI DRAWH DU DV VTOP NCOL U0
                     K J NROW V1 V2 U1 U2 CNT OBS ALL LYR OLDCE OLDOS
                     PVOBJS DIR FDAE I)

  ;; ================= PARAMETERS (edit here) =================
  (setq L        16.48   ; house length along the ridge [m] (Google Earth)
        W         9.40   ; house width eave-to-eave [m] - 9,000 kWh case: widened from 8.14 m
                         ; so three portrait rows fit on the south slope
        H_RIDGE  13.19   ; ridge height above ground [m] (Google Earth)
        TILT     40.0    ; roof tilt [deg] (assumed)
        ROT     104.0    ; final rotation = 90 + 14 -> azimuth -14 deg

        ;; PV module SOLARWATT Panel classic AM 2.5 435
        PA        1.722  ; module length [m]
        PB        1.134  ; module width  [m]
        PT        0.035  ; module thickness [m]
        STANDOFF  0.10   ; gap between roof tiles and module [m]
        GAP       0.02   ; gap between modules [m]

        ;; roof margins
        M_END     0.5    ; margin to gable ends [m]
        M_EAVE    0.4    ; margin to eave [m]
        M_RIDGE   0.4    ; margin to ridge [m]
        CLR       0.2    ; clearance around obstacles [m]

        ;; obstacles in roof coords (u1 u2 v1 v2) - APPROXIMATE,
        ;; check against Google Earth and adjust
        ;; u = 0 is the ENE gable end, u = L is the WSW gable end
        SKY  (list (list  9.5 10.3 1.6 2.8)   ; skylight A
                   (list 11.0 11.8 2.4 3.6))  ; skylight B
        ;; chimney sits just behind the ridge on the NORTH slope
        ;; (v > slope length S = 5.31 m means north of the ridge)
        CHIM (list 1.5 2.1 0.1 0.7)           ; chimney footprint: u1 u2, then v measured BEYOND the ridge
        CHIM_H 1.0                            ; chimney top above ridge [m]
  )
  ;; ==========================================================

  (setq OLDCE (getvar "CMDECHO")
        OLDOS (getvar "OSMODE"))
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)
  (setvar "INSUNITS" 6)
  (command "_.UCS" "_W")
  (setq ALL (ssadd))

  ;; ---------- user input ----------
  (setq N (getint "\nNumber of PV panels <25>: "))
  (if (not N) (setq N 25))
  (initget "Landscape Portrait")
  (setq ORI (getkword "\nPanel orientation [Landscape/Portrait] <Portrait>: "))
  (if (not ORI) (setq ORI "Portrait"))
  (initget "Yes No")
  (setq DRAWH (getkword "\nDraw house, roof and obstacles? [Yes/No] <Yes>: "))
  (if (not DRAWH) (setq DRAWH "Yes"))

  ;; ---------- derived geometry ----------
  (setq TR   (* pi (/ TILT 180.0))
        CS   (cos TR)
        SN   (sin TR)
        RISE (* (/ W 2.0) (/ SN CS))   ; ridge height above eave
        S    (/ (/ W 2.0) CS)          ; slope length eave -> ridge
        HE   (- H_RIDGE RISE)          ; eave height above ground
        CHIM (list (nth 0 CHIM) (nth 1 CHIM) (+ S (nth 2 CHIM)) (+ S (nth 3 CHIM))))  ; chimney always north of ridge

  ;; ---------- house body + gable roof (one solid) ----------
  (if (= DRAWH "Yes")
    (progn
      (pvh:layer "HOUSE" 8)
      (command "_.UCS" "_W")
      ;; UCS whose XY plane is the gable wall (world XZ plane)
      (command "_.UCS" '(0.0 0.0 0.0) '(1.0 0.0 0.0) '(0.0 0.0 1.0))
      (command "_.PLINE"
               '(0.0 0.0)
               (list W 0.0)
               (list W HE)
               (list (/ W 2.0) (+ HE RISE))
               (list 0.0 HE)
               "_C")
      ;; UCS Z points to world -Y, so negative height extrudes along +Y
      (command "_.EXTRUDE" (entlast) "" (- L))
      (ssadd (entlast) ALL)
      ;; record house mesh (10 vertices, 16 triangles)
      (setq *PVH-HOUSE*
        (list (list "House"
          (mapcar 'pvh:rotpt
            (list (list 0.0 0.0 0.0) (list W 0.0 0.0) (list W 0.0 HE)
                  (list (/ W 2.0) 0.0 (+ HE RISE)) (list 0.0 0.0 HE)
                  (list 0.0 L 0.0) (list W L 0.0) (list W L HE)
                  (list (/ W 2.0) L (+ HE RISE)) (list 0.0 L HE)))
          '((0 1 2 "HOUSE") (0 2 3 "HOUSE") (0 3 4 "HOUSE")    ; gable y=0
            (5 9 8 "HOUSE") (5 8 7 "HOUSE") (5 7 6 "HOUSE")    ; gable y=L
            (0 6 1 "HOUSE") (0 5 6 "HOUSE")                    ; floor
            (1 6 7 "HOUSE") (1 7 2 "HOUSE")                    ; north wall
            (0 4 9 "HOUSE") (0 9 5 "HOUSE")                    ; south wall
            (4 3 8 "HOUSE") (4 8 9 "HOUSE")                    ; south roof
            (3 2 7 "HOUSE") (3 7 8 "HOUSE")))))                ; north roof

      ;; chimney (vertical box, pokes through the roof)
      (pvh:layer "OBSTACLES" 1)
      (command "_.UCS" "_W")
      (command "_.BOX"
               (list (* (nth 2 CHIM) CS) (nth 0 CHIM) HE)
               (list (* (nth 3 CHIM) CS) (nth 1 CHIM) HE)
               (+ RISE CHIM_H))
      (ssadd (entlast) ALL)
      (setq *PVH-HOUSE*
        (append *PVH-HOUSE*
          (list (list "Chimney"
            (mapcar 'pvh:rotpt
              (pvh:boxpts (* (nth 2 CHIM) CS) (nth 0 CHIM) HE
                          (* (nth 3 CHIM) CS) (nth 1 CHIM) (+ HE RISE CHIM_H)))
            (pvh:boxtris "OBSTACLE" "OBSTACLE")))))

      ;; skylights (thin boxes lying on the roof)
      (setq I 0)
      (foreach o SKY
        (command "_.BOX"
                 (list (nth 2 o) (nth 0 o) 0.0)
                 (list (nth 3 o) (nth 1 o) 0.0)
                 0.05)
        (pvh:toroof (entlast))
        (ssadd (entlast) ALL)
        (setq I (1+ I)
              *PVH-HOUSE*
              (append *PVH-HOUSE*
                (list (list (strcat "Skylight_" (itoa I))
                  (mapcar '(lambda (p) (pvh:rotpt (pvh:roofpt p)))
                          (pvh:boxpts (nth 2 o) (nth 0 o) 0.0 (nth 3 o) (nth 1 o) 0.05))
                  (pvh:boxtris "OBSTACLE" "OBSTACLE"))))))
    )
  )

  ;; ---------- PV panels ----------
  (if (= ORI "Portrait")
    (setq DU PB DV PA)   ; portrait : short side along ridge
    (setq DU PA DV PB))  ; landscape: long side along ridge
  (setq LYR (strcat "PV_" (itoa N) (if (= ORI "Portrait") "_P" "_L")))
  (pvh:layer LYR 5)
  (command "_.UCS" "_W")
  (setq OBS  (append SKY (list CHIM))
        VTOP (- S M_RIDGE)
        NCOL (fix (/ (+ (- L (* 2.0 M_END)) GAP) (+ DU GAP)))
        U0   (/ (- L (+ (* NCOL DU) (* (1- NCOL) GAP))) 2.0)
        CNT  0
        K    0)

  ;; fill column by column from the ENE gable end (u = 0),
  ;; each column from the ridge down -> one compact block,
  ;; obstacles are skipped
  (setq NROW (fix (/ (+ (- VTOP M_EAVE) GAP) (+ DV GAP)))
        J    0)
  (while (and (< CNT N) (< J NCOL))
    (setq U1 (+ U0 (* J (+ DU GAP)))
          U2 (+ U1 DU)
          K  0)
    (while (and (< CNT N) (< K NROW))
      (setq V2 (- VTOP (* K (+ DV GAP)))
            V1 (- V2 DV))
      (if (not (pvh:hit (list U1 U2 V1 V2) OBS CLR))
        (progn
          (command "_.BOX" (list V1 U1 STANDOFF) (list V2 U2 STANDOFF) PT)
          (pvh:toroof (entlast))
          (ssadd (entlast) ALL)
          (setq CNT (1+ CNT)
                PVOBJS
                (append PVOBJS
                  (list (list (strcat LYR "_" (if (< CNT 10) "0" "") (itoa CNT))
                    ;; PVsyst gets the module TOP SURFACE only: a separate
                    ;; module body would sit at 0 mm from its own PV field
                    ;; and trigger "PV field confused with object" warnings
                    (mapcar '(lambda (p) (pvh:rotpt (pvh:roofpt p)))
                            (list (list V1 U1 (+ STANDOFF PT)) (list V2 U1 (+ STANDOFF PT))
                                  (list V2 U2 (+ STANDOFF PT)) (list V1 U2 (+ STANDOFF PT))))
                    '((0 1 2 "PN_MODULES") (0 2 3 "PN_MODULES"))))))))
      (setq K (1+ K)))
    (setq J (1+ J)))

  ;; ---------- orient the model (azimuth -14 deg) ----------
  (command "_.UCS" "_W")
  (if (> (sslength ALL) 0)
    (command "_.ROTATE" ALL "" '(0.0 0.0 0.0) ROT))
  (command "_.-VIEW" "_SWISO")
  (command "_.ZOOM" "_E")

  ;; ---------- write PVsyst scene (.dae) ----------
  (setq DIR (strcat (getenv "USERPROFILE") "\\Desktop\\PV_HOUSE\\"))
  (vl-mkdir DIR)
  (setq FDAE (strcat DIR "scene_" LYR ".dae"))
  (if (not *PVH-HOUSE*)
    (princ "\n*** House was not drawn in this session - the .dae has panels only. ***"))
  (pvh:writedae FDAE (append *PVH-HOUSE* PVOBJS))

  ;; ---------- report ----------
  (setvar "CMDECHO" OLDCE)
  (setvar "OSMODE" OLDOS)
  (princ (strcat "\n--- PVHOUSE done ---"
                 "\nRoof tilt: " (rtos TILT 2 1) " deg"
                 "   Eave height: " (rtos HE 2 2) " m"
                 "   Ridge height: " (rtos (+ HE RISE) 2 2) " m"
                 "   Slope length: " (rtos S 2 2) " m"
                 "\nPanels placed: " (itoa CNT) " of " (itoa N)
                 " (" ORI ")  ->  " (rtos (* CNT 0.435) 2 3) " kWp"
                 "\nPanel layer: " LYR
                 "\nPVsyst file: " FDAE))
  (if (< CNT N)
    (princ "\n*** WARNING: not enough roof space. Try Portrait or adjust margins/obstacles. ***"))
  (princ)
)

(princ "\nPV_HOUSE loaded. Type PVHOUSE to run.")
(princ)

;;; ===========================================================
;;;  PVEXPORT - export each layer to its own STL file
;;;  Files go to Desktop\PV_HOUSE_STL\ :
;;;     house.stl, obstacles.stl, panels_<layer>.stl
;;;  The whole model is shifted temporarily into the positive
;;;  XYZ octant (STLOUT needs that), exported, then moved back,
;;;  so all STL files stay aligned with each other.
;;; ===========================================================
(defun pvh:bbox (ss / i mn mx a b)
  (setq i 0)
  (repeat (sslength ss)
    (vla-GetBoundingBox (vlax-ename->vla-object (ssname ss i)) 'a 'b)
    (setq a (vlax-safearray->list a)
          b (vlax-safearray->list b)
          mn (if mn (mapcar 'min mn a) a)
          mx (if mx (mapcar 'max mx b) b)
          i (1+ i)))
  (list mn mx)
)

(defun pvh:stl (lyr fname dir / ss f)
  (if (setq ss (ssget "_X" (list (cons 0 "3DSOLID") (cons 8 lyr))))
    (progn
      (setq f (strcat dir fname))
      (if (findfile f) (vl-file-delete f))
      (command "_.STLOUT" ss "" "_Y" f)
      (princ (strcat "\n  " lyr " -> " f
                     "  (" (itoa (sslength ss)) " solids)")))
    (princ (strcat "\n  layer " lyr " is empty - skipped")))
)

(defun c:PVEXPORT ( / OLDFD OLDCE ALLS BB SHIFT DIR LYRS)
  (vl-load-com)
  (setq OLDFD (getvar "FILEDIA")
        OLDCE (getvar "CMDECHO"))
  (setvar "FILEDIA" 0)
  (setvar "CMDECHO" 0)
  (command "_.UCS" "_W")
  (setq DIR (strcat (getenv "USERPROFILE") "\\Desktop\\PV_HOUSE_STL\\"))
  (vl-mkdir DIR)

  (if (setq ALLS (ssget "_X" '((0 . "3DSOLID"))))
    (progn
      ;; shift everything so the minimum corner sits at (1,1,1)
      (setq BB    (pvh:bbox ALLS)
            SHIFT (mapcar '(lambda (v) (- 1.0 v)) (car BB)))
      (command "_.MOVE" ALLS "" '(0.0 0.0 0.0) SHIFT)

      (princ "\nExporting STL files:")
      (pvh:stl "HOUSE"     "house.stl"     DIR)
      (pvh:stl "OBSTACLES" "obstacles.stl" DIR)
      ;; every panel layer (PV_13_L, PV_25_P, ...)
      (setq LYRS (tblnext "LAYER" T))
      (while LYRS
        (if (wcmatch (strcase (cdr (assoc 2 LYRS))) "PV_*")
          (pvh:stl (cdr (assoc 2 LYRS))
                   (strcat "panels_" (cdr (assoc 2 LYRS)) ".stl") DIR))
        (setq LYRS (tblnext "LAYER")))

      ;; move everything back to its original place
      (command "_.MOVE" ALLS "" SHIFT '(0.0 0.0 0.0))
      (princ (strcat "\nDone. Folder: " DIR)))
    (princ "\nNo 3D solids found - run PVHOUSE first."))

  (setvar "FILEDIA" OLDFD)
  (setvar "CMDECHO" OLDCE)
  (princ)
)

(princ "\nType PVEXPORT to export all layers to STL.")
(princ)
