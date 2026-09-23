010042 ID DIVISION.                                                             
030042 PROGRAM-ID.             W4402600.                                        
040042 AUTHOR.                 CARINA VIKTORSSON.                               
050000 DATE-WRITTEN.           JULI 1988.                                       
060000                                                                          
060100                                                                          
070000     REMARKS.                                                             
220042*    VR OCH NOAC FIL SKAPAS MED ORDERBEKRÄFTELSER IFRÅN                   
230042*    ERSÄTTNINGSBEVAKNING RO-REGISTER / LARMKÖ.                           
390042*    FUNKTION:                                                            
400000*                                                                         
660042*        ABENDKODER:                                                      
670042*              U0016 - RETURKOD FRÅN SORT                                 
680000     EJECT                                                                
690042 ENVIRONMENT DIVISION.                                                    
700000                                                                          
710042 INPUT-OUTPUT SECTION.                                                    
720000                                                                          
730042 FILE-CONTROL.                                                            
740000     SKIP2                                                                
750000*    ---- INFIL:                                                          
760000     SELECT  INFIL         ASSIGN  W44026D1.                              
770000     SKIP2                                                                
780000*    ---- UTFILER:                                                        
790000     SELECT  UTFIL-VR      ASSIGN  W44026D2.                              
800042     SELECT  UTFIL-NOAC    ASSIGN  W44026D3.                              
810000     SKIP2                                                                
820042*    ---- SORTFIL:                                                        
830042     SELECT  SORTFIL       ASSIGN  W44026DS.                              
840000     SKIP2                                                                
850042 DATA DIVISION.                                                           
860000                                                                          
870042 FILE SECTION.                                                            
880000     SKIP3                                                                
890000 FD  INFIL                                                                
900042     LABEL RECORD STANDARD                                                
910042     RECORDING  V                                                         
920042     BLOCK CONTAINS 0.                                                    
930000                                                                          
940031 01  FILLER      PIC X(998).                                              
941042 01  INPOST -COPY W440001     -L.                                         
950000     EJECT                                                                
960000 FD  UTFIL-VR                                                             
970042     LABEL RECORD STANDARD                                                
980042     RECORDING  V                                                         
990042     BLOCK CONTAINS 0                                                     
000042     DATA RECORD ARE PSU2 PSU3 PSU5 PSUD.                                 
010000                                                                          
020042 01  PSU2 -COPY W425SU2       -L.                                         
030000                                                                          
040042 01  PSU3 -COPY W425SU3       -L.                                         
050000                                                                          
060042 01  PSU5 -COPY W425SU5       -L.                                         
070000                                                                          
080042 01  PSUD -COPY W425SUD       -L.                                         
090000     EJECT                                                                
100042 FD  UTFIL-NOAC                                                           
110042     LABEL RECORD STANDARD                                                
120042     RECORDING  V                                                         
130042     BLOCK CONTAINS 0                                                     
140042     DATA RECORD ARE P002 P003 P004 P005 P008.                            
150000                                                                          
160042 01  P002 -COPY W461002       -L.                                         
170000                                                                          
180042 01  P003 -COPY W461003       -L.                                         
190000                                                                          
200042 01  P004 -COPY W461004       -L.                                         
210000                                                                          
220042 01  P005 -COPY W461005       -L.                                         
230000                                                                          
240042 01  P008 -COPY W461008       -L.                                         
250000     EJECT                                                                
260042 SD  SORTFIL                                                              
270042     RECORDING F.                                                         
280042 01  SORT-POST -COPY W440001                                              
290000     EJECT                                                                
300042 WORKING-STORAGE SECTION.                                                 
300132                                                                          
301032*    -- CHECKED BY WY2000                                                 
310042 77  PROGRAM-NAMN            PIC X(8)    VALUE 'W4402600'.                
320000     SKIP2                                                                
330000 77  JA                      PIC X       VALUE 'J'.                       
340000 77  NEJ                     PIC X       VALUE 'N'.                       
341020 77  FELTEXT                 PIC X(32)   VALUE SPACE.                     
350042 77  C1                      PIC S9(1)   COMP-3 VALUE +1.                 
360000     SKIP2                                                                
370042 77  INFIL-EOF               PIC X       VALUE 'N'.                       
380042 77  SORTFIL-EOF             PIC X       VALUE 'N'.                       
390000     SKIP2                                                                
400042 77  SPAR-IDARTNR            PIC S9(9)   COMP-3 VALUE ZERO.               
410000     SKIP2                                                                
420042 77  IDDISTR-BRYT            PIC S9(5)   COMP-3 VALUE ZERO.               
430042 77  IDKUNDNR-BRYT           PIC S9(7)   COMP-3 VALUE ZERO.               
440000 77  IDKUNDRF-BRYT           PIC X(10)   VALUE SPACE.                     
450000 77  TIAAMMDD                PIC 9(6).                                    
460042 77  TIKLOCK                 PIC 9(8).                                    
470000     SKIP2                                                                
480042 01  ORDERNR-WS              PIC X(10).                                   
490042 01  ORDNR REDEFINES ORDERNR-WS.                                          
500042     03  IDORDNR-WS          PIC S9(5).                                   
510000     03  FILLER              PIC X(5).                                    
520000     SKIP2                                                                
530000 01  SU-WS.                                                               
560042     03  KDVRTPO-SU-WS       PIC 9(1).                                    
570000     SKIP2                                                                
580000 01  SW-SKAPA-002            PIC X(1).                                    
590000     88  SKAPA-002-JA        VALUE 'J'.                                   
600000     88  SKAPA-002-NEJ       VALUE 'N'.                                   
610000     EJECT                                                                
620042*    ---- SUBPROGRAM OCH PARAMETER-AREOR                                  
630000     SKIP3                                                                
640042 01  DYNAMISKA-SUBPROGRAM.                                                
650042   03  POSTSUM               PIC X(8)    VALUE 'POSTSUM '.                
660000   03  ABEND                 PIC X(8)    VALUE 'ABEND   '.                
670042   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
680042   03  WDATKONV              PIC X(8)    VALUE 'WDATKONV'.                
691009   03  W460DIS1              PIC X(8)    VALUE 'W460DIS1'.                
700000     SKIP3                                                                
710000*    ----  PARAMETRAR TILL ABEND                                          
720000                                                                          
730042 01  RKOD-ABEND-UTAN-DUMP    PIC S9(4) VALUE +16 COMP SYNC.               
731042 01  RKOD-ABEND-MED-DUMP     PIC S9(4) VALUE +33 COMP SYNC.               
740000     EJECT                                                                
750042*    ----  PARAMETRAR TILL DATKONV                                        
760000                                                                          
770042 01  -COPY WDATAREA                                                       
780000     EJECT                                                                
790042*    ----  PARAMETRAR TILL POSTSUM                                        
800000                                                                          
810042 01  -COPY W0005       -PRE POSTSUM-.                                     
820000     EJECT                                                                
861009*    ----  PARAMETRAR TILL W460DIS1                                       
862009                                                                          
863042 01  -COPY W460DIS1                                                       
864009     EJECT                                                                
870042 01  TEST-IDDISTR           PIC 9(5)   COMP-3.                            
880042 01  FILLER  -COPY WWDIS130    -RED TEST-IDDISTR                          
890000     EJECT                                                                
951042 01  FILLER  -COPY WWDIST24    -RED TEST-IDDISTR                          
952015     EJECT                                                                
960042*    ----  AREA FÖR UTPOSTER                                              
970042 01  FILLER                  PIC X(24) VALUE 'AREA UTPOSTER '.            
980000     SKIP3                                                                
990000 01  FILLER                  PIC X(8)  VALUE 'SU2*****'.                  
000042 01  -COPY W425SU2       -PRE SU2-                                        
010000     EJECT                                                                
020000 01  FILLER                  PIC X(8)  VALUE 'SU3*****'.                  
030042 01  -COPY W425SU3       -PRE SU3-                                        
040000     EJECT                                                                
050000 01  FILLER                  PIC X(8)  VALUE 'SU5*****'.                  
060042 01  -COPY W425SU5       -PRE SU5-                                        
070000     EJECT                                                                
080000 01  FILLER                  PIC X(8)  VALUE 'SUD*****'.                  
090042 01  -COPY W425SUD       -PRE SUD-                                        
100000     EJECT                                                                
110000 01  FILLER                  PIC X(8)  VALUE '002*****'.                  
120042 01  -COPY W461002                                                        
130000     EJECT                                                                
140000 01  FILLER                  PIC X(8)  VALUE '003*****'.                  
150042 01  -COPY W461003                                                        
160000     EJECT                                                                
170000 01  FILLER                  PIC X(8)  VALUE '004*****'.                  
180042 01  -COPY W461004                                                        
190000     EJECT                                                                
200000 01  FILLER                  PIC X(8)  VALUE '005*****'.                  
210042 01  -COPY W461005                                                        
220000     EJECT                                                                
230000 01  FILLER                  PIC X(8)  VALUE '008*****'.                  
240042 01  -COPY W461008                                                        
250000     EJECT                                                                
260042 01  W-R241-POST.                                                         
270000     03  W-R241-IDDISTR     PIC 9(4).                                     
280042     03  W-R241-IDORDNR     PIC 9(5).                                     
290000     03  W-R241-IDARTNR     PIC 9(7).                                     
300000     03  FILLER             PIC XX      VALUE SPACE.                      
310000     03  W-R241-ID          PIC XX.                                       
320042     03  W-R241-KVREL       PIC S9(7)   COMP-3.                           
330000     03  FILLER             PIC X(17)   VALUE SPACE.                      
340000     EJECT                                                                
350000     EJECT                                                                
360042 PROCEDURE DIVISION.                                                      
370000     SKIP2                                                                
380042     PERFORM A-INIT                                                       
390000                                                                          
400042     SORT SORTFIL                                                         
410042        ASCENDING FOR-IDDISTR FOR-IDKUNDNR FOR-IDKUNDRF                   
420042                  FOR-IDLOPNRE FOR-IDKORTNR-ERS                           
430042        INPUT  PROCEDURE B-LAS-POSTER                                     
440042        OUTPUT PROCEDURE C-SKAPA-UTPOSTER.                                
450000                                                                          
460042     IF SORT-RETURN > ZERO                                                
470042       DISPLAY '*** W44026 - FEL VID SORTERING'                           
480042       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
490000     ELSE                                                                 
500042       PERFORM Z-FINIT                                                    
510042       MOVE ZERO TO RETURN-CODE                                           
520042       GOBACK                                                             
530000     END-IF                                                               
540000     .                                                                    
550000     EJECT                                                                
560042 A-INIT SECTION.                                                          
570000     SKIP2                                                                
580042     OPEN INPUT  INFIL                                                    
590042     OPEN OUTPUT UTFIL-VR                                                 
600042                 UTFIL-NOAC                                               
610000                                                                          
620042     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
630042     ACCEPT TIAAMMDD FROM DATE                                            
640042     ACCEPT TIKLOCK  FROM TIME                                            
650000     .                                                                    
660000     EJECT                                                                
670042 B-LAS-POSTER SECTION.                                                    
680000     SKIP2                                                                
690042     PERFORM BA-LAS-POST                                                  
700042     PERFORM UNTIL INFIL-EOF = JA                                         
710042        RELEASE SORT-POST                                                 
720042        PERFORM BA-LAS-POST                                               
730042     END-PERFORM                                                          
740000     .                                                                    
750000     EJECT                                                                
760042 BA-LAS-POST SECTION.                                                     
770000     SKIP2                                                                
780042     READ INFIL INTO SORT-POST                                            
790042       AT END MOVE JA TO INFIL-EOF                                        
800000     END-READ                                                             
810000                                                                          
820042     IF INFIL-EOF = NEJ                                                   
830042       MOVE 'W44026'         TO POSTSUM-FDNAMN                            
840042       MOVE 'W44026D1'       TO POSTSUM-DDNAMN2                           
850042       MOVE 'IN'             TO POSTSUM-TRANSTYP                          
860042       CALL POSTSUM USING POSTSUM-PARM                                    
870000     END-IF                                                               
880000     .                                                                    
890000     EJECT                                                                
900042 C-SKAPA-UTPOSTER SECTION.                                                
910000     SKIP2                                                                
920042     MOVE NEJ TO SW-SKAPA-002                                             
930042     PERFORM CA-LAS-SORTPOST                                              
940000                                                                          
950042     PERFORM UNTIL SORTFIL-EOF = JA                                       
960000                                                                          
970042       MOVE NEJ TO SW-SKAPA-002                                           
980042       MOVE FOR-IDDISTR  TO IDDISTR-BRYT                                  
990042       MOVE FOR-IDKUNDNR TO IDKUNDNR-BRYT                                 
000042       MOVE FOR-IDKUNDRF TO IDKUNDRF-BRYT                                 
010000                                                                          
020042       PERFORM UNTIL (SORTFIL-EOF = JA                                    
030042                  OR  FOR-IDDISTR  NOT = IDDISTR-BRYT                     
040042                  OR  FOR-IDKUNDNR NOT = IDKUNDNR-BRYT                    
050042                  OR  FOR-IDKUNDRF NOT = IDKUNDRF-BRYT)                   
060000                                                                          
070042         MOVE FOR-IDDISTR TO TEST-IDDISTR DIS1-IDDISTR                    
080042         MOVE FOR-IDKUNDRF TO ORDERNR-WS                                  
090000                                                                          
091008         CALL W460DIS1 USING DIS1-W460DIS1                                
092008                                                                          
100000         EVALUATE TRUE                                                    
110000                                                                          
120042           WHEN FOR-KDRADERS = +1                                         
130042             PERFORM CB-ERSATT-RAD                                        
140000                                                                          
150042           WHEN FOR-KDTILLK = +1                                          
160042             PERFORM CC-TILLKOMMANDE-RAD                                  
170000                                                                          
180042           WHEN OTHER                                                     
190142             DISPLAY 'LASSI**HÄR FANNS CD-LTK-SECTION'                    
190242             DISPLAY 'HIT BORDE VI INTE HAMNAT!!!'                        
190342             MOVE 'LASSIS KOMM. SE DISPLAYER NEDAN'                       
190442                         TO FELTEXT                                       
191042             CALL ABEND USING RKOD-ABEND-MED-DUMP                         
200000                                                                          
210000         END-EVALUATE                                                     
220000                                                                          
230042         PERFORM CA-LAS-SORTPOST                                          
240042       END-PERFORM                                                        
250000                                                                          
260000       IF  SKAPA-002-JA                                                   
270042         WRITE P002 FROM OBHUV-W461002                                    
280042         PERFORM S02-UTPOST-NOAC                                          
290000       END-IF                                                             
300000                                                                          
310042     END-PERFORM                                                          
320000     .                                                                    
330000     EJECT                                                                
340042 CA-LAS-SORTPOST SECTION.                                                 
350000     SKIP2                                                                
360042     RETURN SORTFIL                                                       
370042        AT END MOVE JA TO SORTFIL-EOF                                     
380000     END-RETURN                                                           
390000                                                                          
400042     IF SORTFIL-EOF = NEJ                                                 
410042        MOVE 'W44026'          TO POSTSUM-FDNAMN                          
420042        MOVE 'W44026DS'        TO POSTSUM-DDNAMN2                         
430042        MOVE 'SORT'            TO POSTSUM-TRANSTYP                        
440042        CALL POSTSUM USING POSTSUM-PARM                                   
450000     END-IF                                                               
460000     .                                                                    
470000     EJECT                                                                
480042 CB-ERSATT-RAD SECTION.                                                   
490000     SKIP2                                                                
500042     MOVE FOR-IDARTNR        TO SPAR-IDARTNR                              
510000                                                                          
511042     IF NOT DIST24-NORGE                                                  
520042       PERFORM CBA-VR-ERSATT-RAD                                          
521014     END-IF                                                               
530000                                                                          
540042     IF (DIS130-NOAC OR DIS1-KDSVAR = JA)                                 
550042       PERFORM S04-SKAPA-002-NOAC                                         
560042       MOVE JA TO SW-SKAPA-002                                            
570000                                                                          
580000       EVALUATE TRUE                                                      
590000                                                                          
600042         WHEN FOR-KDRESTR = +61                                           
610042           PERFORM S07-SKAPA-004-ERS-DEL-NOAC                             
620042           PERFORM S08-SKAPA-005-ERS-DEL-NOAC                             
630000                                                                          
640042         WHEN FOR-KDRESTR = +41                                           
650042           PERFORM S12-SKAPA-003-ERS-DEL-NOAC                             
660000                                                                          
670042         WHEN OTHER                                                       
680042           PERFORM S13-SKAPA-SKRIV-008-NOAC                               
690000                                                                          
700000       END-EVALUATE                                                       
710000                                                                          
720000     END-IF                                                               
730000     .                                                                    
740000     EJECT                                                                
750042 CBA-VR-ERSATT-RAD SECTION.                                               
760000     SKIP2                                                                
770000     EVALUATE TRUE                                                        
780000                                                                          
790042       WHEN  FOR-KDSTARAD >= '2'                                          
800042         OR   (FOR-KDSTARAD = '1' AND FOR-KDTPOTYP = 6)                   
870000                                                                          
920000         EVALUATE TRUE                                                    
930000                                                                          
940042           WHEN FOR-KDRESTR = +61                                         
950044             MOVE IDORDNR-WS TO SU3-IDORDNR-002                           
960045             MOVE FOR-KVART  TO SU3-KVBEART-002                           
970042             PERFORM S33-SU3-RED-SKR                                      
980000                                                                          
990042           WHEN FOR-KDRESTR = +41                                         
000000                                                                          
010044             MOVE IDORDNR-WS TO SU2-IDORDNR-002                           
020045             MOVE FOR-KVART  TO SU2-KVBEART-002                           
030042             PERFORM S30-SU2-RED-ERS-DEL                                  
040000                                                                          
050042           WHEN OTHER                                                     
060044             MOVE IDORDNR-WS TO SU2-IDORDNR-002                           
070045             MOVE FOR-KVART  TO SU2-KVBEART-002                           
080042             PERFORM S30-SU2-RED-ERS-DEL                                  
090042             PERFORM S32-SU2-INIT-TIL-DEL-SKR                             
100000                                                                          
110000         END-EVALUATE                                                     
120000                                                                          
130042       WHEN  OTHER                                                        
140000                                                                          
150042*        -----  TPO TYP=1-5  (FÖRUTOM VISSA TYP=2)                        
160000                                                                          
170000         EVALUATE TRUE                                                    
180000                                                                          
190042           WHEN FOR-KDRESTR = +61                                         
200042*            -----  SU3 MED NOLL I ORDNR & ANTAL                          
210044             MOVE ZERO       TO SU3-IDORDNR-002                           
220045             MOVE ZERO       TO SU3-KVBEART-002                           
230042             PERFORM S33-SU3-RED-SKR                                      
240000                                                                          
250042           WHEN FOR-KDRESTR = +41                                         
260042*            -----  SU2 ERS-DEL MED NOLL I ORDNR & ANTAL                  
270044             MOVE ZERO       TO SU2-IDORDNR-002                           
280045             MOVE ZERO       TO SU2-KVBEART-002                           
290042             PERFORM S30-SU2-RED-ERS-DEL                                  
300000                                                                          
310042           WHEN OTHER                                                     
320042*            -----  SU2 MED IFYLLD ERS-DEL OCH NOLLAD TIL-DEL             
330044             MOVE IDORDNR-WS TO SU2-IDORDNR-002                           
340045             MOVE FOR-KVART  TO SU2-KVBEART-002                           
350042             PERFORM S30-SU2-RED-ERS-DEL                                  
360042             PERFORM S32-SU2-INIT-TIL-DEL-SKR                             
370000                                                                          
380000         END-EVALUATE                                                     
390000                                                                          
400042*        -----  SUD MED NOLL I ANTAL                                      
410045         MOVE ZERO           TO SUD-KVBEART-002                           
420042         PERFORM S35-SUD-RED-SKR                                          
430000                                                                          
440000     END-EVALUATE                                                         
450000     .                                                                    
460000     EJECT                                                                
470042 CC-TILLKOMMANDE-RAD SECTION.                                             
480000     SKIP2                                                                
481042     IF NOT DIST24-NORGE                                                  
490042       PERFORM CCA-VR-TILLK-RAD                                           
491015     END-IF                                                               
500000                                                                          
510042     IF  (DIS130-NOAC OR DIS1-KDSVAR = JA)                                
520042     AND FOR-KDSTARAD NOT = '3'                                           
530000                                                                          
540042       IF FOR-KDRESTR = +61                                               
550042          IF  FOR-BEERS = SPACE                                           
560042          AND FOR-IDARTNR > +0                                            
570042*           -----  NORMAL POST                                            
580042            PERFORM S16-SKAPA-TILL-DEL-004-NOAC                           
590000          ELSE                                                            
600042*           -----  TEXT-POST                                              
610042            PERFORM S17-SKAPA-TILL-DEL-005-NOAC                           
620000          END-IF                                                          
630000                                                                          
640000       ELSE                                                               
650042         PERFORM S19-SKAPA-TILL-DEL-003-NOAC                              
660000                                                                          
670042         IF  FOR-KDRESTR = +53 OR +58 OR +67                              
680042           PERFORM S13-SKAPA-SKRIV-008-NOAC                               
690000         END-IF                                                           
700000       END-IF                                                             
710000     END-IF                                                               
720000     .                                                                    
730000     EJECT                                                                
740042 CCA-VR-TILLK-RAD SECTION.                                                
750000     SKIP2                                                                
760000     EVALUATE TRUE                                                        
770000                                                                          
780042       WHEN FOR-KDRESTR = +61                                             
790000                                                                          
800042         CONTINUE                                                         
810000                                                                          
820042       WHEN FOR-KDRESTR = +53 OR +58 OR +67                               
830042*        -----  SU3 MED ORDNR & ANTAL IFYLLDA                             
840044         MOVE IDORDNR-WS     TO SU3-IDORDNR-002                           
850045         MOVE FOR-KVART      TO SU3-KVBEART-002                           
860042         PERFORM S33-SU3-RED-SKR                                          
870000                                                                          
880042       WHEN OTHER                                                         
890000                                                                          
900000         EVALUATE TRUE                                                    
910000                                                                          
920042           WHEN FOR-KDSTARAD = '2'                                        
930042*            -----  RO                                                    
940045             MOVE FOR-KVART  TO SU2-KVLEVART-002                          
950042             PERFORM S31-SU2-RED-TIL-DEL-SKR                              
960000                                                                          
970042           WHEN FOR-KDSTARAD = '3'                                        
980042*            -----  TÄCKT RO                                              
990000                                                                          
000042             IF  (FOR-KDRESTR = +0 OR +47)                                
010042             AND (FOR-FLVR = +1)                                          
030042               PERFORM S34-SU5-RED-SKR                                    
040000             END-IF                                                       
050000                                                                          
060042           WHEN OTHER                                                     
080000                                                                          
090042             IF  FOR-KDTPOTYP = 6                                         
180000                                                                          
190045               MOVE FOR-KVART TO SU2-KVLEVART-002                         
200042               PERFORM S31-SU2-RED-TIL-DEL-SKR                            
210000                                                                          
220000             ELSE                                                         
230042*              -----  TPO TYP=1-5  (FÖRUTOM VISSA TYP=2)                  
240001                                                                          
250045               MOVE ZERO      TO SU2-KVLEVART-002                         
260042               PERFORM S31-SU2-RED-TIL-DEL-SKR                            
270000                                                                          
280045               MOVE FOR-KVART TO SUD-KVBEART-002                          
290042               PERFORM S35-SUD-RED-SKR                                    
300000             END-IF                                                       
310000                                                                          
320000         END-EVALUATE                                                     
330000                                                                          
340000     END-EVALUATE                                                         
350000     .                                                                    
360000     EJECT                                                                
010042 Z-FINIT SECTION.                                                         
020000     SKIP2                                                                
030042     CLOSE  INFIL                                                         
040000            UTFIL-VR                                                      
050042            UTFIL-NOAC                                                    
060000                                                                          
070042     MOVE 'S' TO POSTSUM-OPKOD                                            
080042     CALL POSTSUM USING POSTSUM-PARM                                      
090000     .                                                                    
100000     EJECT                                                                
110042 S01-UTPOST-VR SECTION.                                                   
120000     SKIP2                                                                
130042     MOVE 'W44026'           TO POSTSUM-FDNAMN                            
140042     MOVE 'W44026D2'         TO POSTSUM-DDNAMN2                           
150042     MOVE 'VR'               TO POSTSUM-TRANSTYP                          
160042     CALL POSTSUM USING POSTSUM-PARM                                      
170000     .                                                                    
180000     EJECT                                                                
190042 S02-UTPOST-NOAC SECTION.                                                 
200000     SKIP2                                                                
210042     MOVE 'W44026'           TO POSTSUM-FDNAMN                            
220042     MOVE 'W44026D3'         TO POSTSUM-DDNAMN2                           
230042     MOVE 'NOAC'             TO POSTSUM-TRANSTYP                          
240042     CALL POSTSUM USING POSTSUM-PARM                                      
250000     .                                                                    
260000     EJECT                                                                
270042 S04-SKAPA-002-NOAC SECTION.                                              
280000     SKIP2                                                                
290042     MOVE '002'              TO OBHUV-IDPTYP                              
300042     MOVE FOR-IDDISTR        TO OBHUV-IDDISTR                             
310042     MOVE FOR-IDKUNDNR       TO OBHUV-IDKUNDNR                            
320042     MOVE FOR-KDFRAKT        TO OBHUV-KDFRAKT                             
330042     MOVE IDORDNR-WS         TO OBHUV-IDORDNR                             
340042     MOVE FOR-BEVOLREF       TO OBHUV-BEVOLREF                            
350042     MOVE FOR-BEVARREF       TO OBHUV-BEVARREF                            
360042     MOVE FOR-KDORDKL        TO OBHUV-KDORDKL                             
370042     MOVE FOR-TIREGDAT       TO OBHUV-TIORDREG                            
380042     MOVE FOR-KDFAKTYP       TO OBHUV-KDFAKTYP                            
390000     .                                                                    
400000     EJECT                                                                
410042 S07-SKAPA-004-ERS-DEL-NOAC SECTION.                                      
420000     SKIP2                                                                
430042     MOVE '004'              TO OBEEN-IDPTYP                              
440042     MOVE FOR-IDDISTR        TO OBEEN-IDDISTR                             
450042     MOVE FOR-IDKUNDNR       TO OBEEN-IDKUNDNR                            
460042     MOVE FOR-KDFRAKT        TO OBEEN-KDFRAKT                             
470042     MOVE IDORDNR-WS         TO OBEEN-IDORDNR                             
480042                                OBEEN-IDRONR                              
490042     MOVE +0                 TO OBEEN-KDLIDEL                             
500042     MOVE FOR-IDDC           TO OBEEN-IDDC                                
510042     MOVE FOR-IDARTNR        TO OBEEN-IDARTNR                             
520042     MOVE FOR-REKSIFFR       TO OBEEN-REKSIFFR                            
530042     MOVE FOR-BEART          TO OBEEN-BEART                               
540042     MOVE FOR-BERADREF       TO OBEEN-BERADREF                            
550042     MOVE FOR-TIRODAT        TO OBEEN-TIRODAT                             
560042     MOVE FOR-BEVOLREF       TO OBEEN-BEVOLREF                            
570042     MOVE FOR-KDRESTR        TO OBEEN-KDRESTR                             
580042     IF FOR-KDERS > +10                                                   
590042        MOVE FOR-KDERS       TO OBEEN-KDERS                               
600000     ELSE                                                                 
610042        MOVE +0              TO OBEEN-KDERS                               
620000     END-IF                                                               
630042     IF FOR-KDERS < +10 OR                                                
640042        FOR-KDERS = +17 OR +19 OR +27 OR +18 OR +28 OR +29 OR +52         
650042        MOVE +0              TO OBEEN-KDERSUP                             
660000     ELSE                                                                 
670042        MOVE +1              TO OBEEN-KDERSUP                             
680000     END-IF                                                               
690042     MOVE FOR-KVART          TO OBEEN-KVBEART                             
700042     MOVE FOR-IDLOPNRE       TO OBEEN-IDLOPNRE                            
710042     MOVE FOR-KDKVBRYT       TO OBEEN-KDKVBRYT                            
720042     MOVE FOR-KDDSP          TO OBEEN-KDDSP                               
730042     MOVE FOR-KDFAKTYP       TO OBEEN-KDFAKTYP                            
740000     .                                                                    
750000     EJECT                                                                
760042 S08-SKAPA-005-ERS-DEL-NOAC SECTION.                                      
770000     SKIP2                                                                
780042     MOVE '005'              TO OBTEXT-IDPTYP                             
790042     MOVE FOR-IDDISTR        TO OBTEXT-IDDISTR                            
800042     MOVE FOR-IDKUNDNR       TO OBTEXT-IDKUNDNR                           
810042     MOVE FOR-KDFRAKT        TO OBTEXT-KDFRAKT                            
820042     MOVE IDORDNR-WS         TO OBTEXT-IDORDNR                            
830042                                OBTEXT-IDRONR                             
840042     MOVE +0                 TO OBTEXT-KDLIDEL                            
850042     MOVE FOR-IDDC           TO OBTEXT-IDDC                               
860042     MOVE FOR-IDARTNR        TO OBTEXT-IDARTNR                            
870042     MOVE FOR-REKSIFFR       TO OBTEXT-REKSIFFR                           
880042     MOVE FOR-BEART          TO OBTEXT-BEART                              
890042     MOVE FOR-BERADREF       TO OBTEXT-BERADREF                           
900042     MOVE FOR-TIRODAT        TO OBTEXT-TIRODAT                            
910042     MOVE FOR-BEVOLREF       TO OBTEXT-BEVOLREF                           
920042     MOVE FOR-KDRESTR        TO OBTEXT-KDRESTR                            
930042     IF FOR-KDERS > +10                                                   
940042        MOVE FOR-KDERS       TO OBTEXT-KDERS                              
950000     ELSE                                                                 
960042        MOVE +0              TO OBTEXT-KDERS                              
970000     END-IF                                                               
980042     IF FOR-KDERS < +10 OR                                                
990042        FOR-KDERS = +17 OR +19 OR +27 OR +28 OR +29 OR +52                
000042        MOVE +0              TO OBTEXT-KDERSUP                            
010000     ELSE                                                                 
020042        MOVE +1              TO OBTEXT-KDERSUP                            
030000     END-IF                                                               
040042     MOVE FOR-KVART          TO OBTEXT-KVBEART                            
050042     MOVE FOR-IDLOPNRE       TO OBTEXT-IDLOPNRE                           
060042     MOVE FOR-KDKVBRYT       TO OBTEXT-KDKVBRYT                           
070042     MOVE FOR-KDDSP          TO OBTEXT-KDDSP                              
080042     MOVE FOR-KDFAKTYP       TO OBTEXT-KDFAKTYP                           
090000     .                                                                    
100000     EJECT                                                                
110042 S12-SKAPA-003-ERS-DEL-NOAC SECTION.                                      
120000     SKIP2                                                                
130042     MOVE '003'              TO OBEN-IDPTYP                               
140042     MOVE FOR-IDDISTR        TO OBEN-IDDISTR                              
150042     MOVE FOR-IDKUNDNR       TO OBEN-IDKUNDNR                             
160042     MOVE FOR-KDFRAKT        TO OBEN-KDFRAKT                              
170042     MOVE IDORDNR-WS         TO OBEN-IDORDNR                              
180042                                OBEN-IDRONR                               
190042     MOVE +0                 TO OBEN-KDLIDEL                              
200042     MOVE FOR-IDDC           TO OBEN-IDDC                                 
210042     MOVE FOR-IDARTNR        TO OBEN-IDARTNR                              
220042     MOVE FOR-REKSIFFR       TO OBEN-REKSIFFR                             
230042     MOVE FOR-BEART          TO OBEN-BEART                                
240042     MOVE FOR-BERADREF       TO OBEN-BERADREF                             
250042     MOVE FOR-TIRODAT        TO OBEN-TIRODAT                              
260042     MOVE FOR-BEVOLREF       TO OBEN-BEVOLREF                             
270042     MOVE FOR-KDRESTR        TO OBEN-KDRESTR                              
280042     IF FOR-KDERS > +10                                                   
290042        MOVE FOR-KDERS       TO OBEN-KDERS                                
300000     ELSE                                                                 
310042        MOVE +0              TO OBEN-KDERS                                
320000     END-IF                                                               
330042     IF FOR-KDERS < +10 OR                                                
340042        FOR-KDERS = +17 OR +19 OR +27 OR +28 OR +29 OR +52                
350042        MOVE +0              TO OBEN-KDERSUP                              
360000     ELSE                                                                 
370042        MOVE +1              TO OBEN-KDERSUP                              
380000     END-IF                                                               
390042     MOVE FOR-KVART          TO OBEN-KVBEART                              
400042     MOVE FOR-IDLOPNRE       TO OBEN-IDLOPNRE                             
410042     MOVE FOR-KDKVBRYT       TO OBEN-KDKVBRYT                             
420042     MOVE FOR-KDDSP          TO OBEN-KDDSP                                
430042     MOVE FOR-KDFAKTYP       TO OBEN-KDFAKTYP                             
440000     .                                                                    
450000     EJECT                                                                
460042 S13-SKAPA-SKRIV-008-NOAC SECTION.                                        
470000     SKIP2                                                                
480042     MOVE '008'              TO OBSTOP-IDPTYP                             
490042     MOVE FOR-IDDISTR        TO OBSTOP-IDDISTR                            
500042     MOVE FOR-IDKUNDNR       TO OBSTOP-IDKUNDNR                           
510042     MOVE FOR-KDFRAKT        TO OBSTOP-KDFRAKT                            
520042     MOVE IDORDNR-WS         TO OBSTOP-IDORDNR                            
530042                                OBSTOP-IDRONR                             
540042     MOVE +0                 TO OBSTOP-KDLIDEL                            
550042     MOVE FOR-IDDC           TO OBSTOP-IDDC                               
560042     MOVE FOR-IDARTNR        TO OBSTOP-IDARTNR                            
570042     MOVE FOR-REKSIFFR       TO OBSTOP-REKSIFFR                           
580042     MOVE FOR-BEART          TO OBSTOP-BEART                              
590042     MOVE FOR-BERADREF       TO OBSTOP-BERADREF                           
600042     MOVE FOR-TIRODAT        TO OBSTOP-TIRODAT                            
610042     MOVE FOR-BEVOLREF       TO OBSTOP-BEVOLREF                           
620042     MOVE FOR-KDRESTR        TO OBSTOP-KDRESTR                            
630042     MOVE FOR-TIREGDAT       TO OBSTOP-TIORDREG                           
640042     MOVE FOR-KVART          TO OBSTOP-KVBEART                            
650042     MOVE FOR-KDKVBRYT       TO OBSTOP-KDKVBRYT                           
660042     MOVE FOR-KDDSP          TO OBSTOP-KDDSP                              
670042     MOVE FOR-KDFAKTYP       TO OBSTOP-KDFAKTYP                           
671042     MOVE TIAAMMDD           TO OBSTOP-TIAAMMDD                           
672042     MOVE TIKLOCK            TO OBSTOP-TIKLOCK                            
680042     WRITE P008 FROM OBSTOP-W461008                                       
690042     PERFORM S02-UTPOST-NOAC                                              
700000     .                                                                    
710000     EJECT                                                                
720042 S16-SKAPA-TILL-DEL-004-NOAC SECTION.                                     
730000     SKIP2                                                                
740042     MOVE FOR-IDKORTNR-ERS   TO OBEEN-IDKORTNR                            
750042     MOVE FOR-IDARTNR        TO OBEEN-IDARTNR-TILLK                       
760042     MOVE FOR-REKSIFFR       TO OBEEN-REKSIFFR-TILLK                      
770042     MOVE FOR-KVART          TO OBEEN-KVBEART-TILLK                       
780042     COMPUTE OBEEN-DIERS-KVOT ROUNDED =                                   
790042             FOR-DIERS-TILLK / FOR-DIERS-ERS                              
800042             ON SIZE ERROR MOVE +0 TO OBEEN-DIERS-KVOT                    
810042     END-COMPUTE                                                          
820042     MOVE +2                 TO OBEEN-KDRO                                
830042     WRITE P004 FROM OBEEN-W461004                                        
840042     PERFORM S02-UTPOST-NOAC                                              
850000     .                                                                    
860000     EJECT                                                                
870042 S17-SKAPA-TILL-DEL-005-NOAC SECTION.                                     
880000     SKIP2                                                                
890042     MOVE FOR-IDKORTNR-ERS   TO OBTEXT-IDKORTNR                           
900042     MOVE FOR-BEERS          TO OBTEXT-BEERS                              
910042     MOVE +2                 TO OBTEXT-KDRO                               
920042     WRITE P005 FROM OBTEXT-W461005                                       
930042     PERFORM S02-UTPOST-NOAC                                              
940000     .                                                                    
950000     EJECT                                                                
960042 S19-SKAPA-TILL-DEL-003-NOAC SECTION.                                     
970000     SKIP2                                                                
980042     MOVE FOR-IDKORTNR-ERS   TO OBEN-IDKORTNR                             
990042     MOVE FOR-IDARTNR        TO OBEN-IDARTNR-TILLK                        
000042     MOVE FOR-REKSIFFR       TO OBEN-REKSIFFR-TILLK                       
010042     MOVE FOR-BEART          TO OBEN-BEART-TILLK                          
020042     MOVE FOR-KVART          TO OBEN-KVBEART-TILLK                        
030042     COMPUTE OBEN-DIERS-KVOT ROUNDED =                                    
040042             FOR-DIERS-TILLK / FOR-DIERS-ERS                              
050042             ON SIZE ERROR MOVE +0 TO OBEN-DIERS-KVOT                     
060042     END-COMPUTE                                                          
070042     MOVE +2                 TO OBEN-KDRO                                 
080042     MOVE +2                 TO OBEN-KDLIDEL                              
090042     WRITE P003 FROM OBEN-W461003                                         
100042     PERFORM S02-UTPOST-NOAC                                              
110000     .                                                                    
120000     EJECT                                                                
130042 S30-SU2-RED-ERS-DEL SECTION.                                             
140000*                                                                         
150000*    SU2 - REDIGERA ERSATT-DEL                                            
160044*    FÄLTEN IDORDNR-002 OCH KVBEART REDIGERAS                             
170042*    UTANFÖR DENNA SECTION.                                               
180000*                                                                         
190042     MOVE 'SU2'              TO SU2-IDPTYP                                
200000                                                                          
240042     MOVE FOR-IDDISTR        TO SU2-IDDISTR                               
241042     MOVE FOR-IDKUNDNR       TO SU2-IDKUNDNR                              
250042     MOVE 711                TO SU2-IDSUPPL                               
260042     MOVE FOR-IDARTNR        TO SU2-IDARTNR-ERS                           
270042     MOVE FOR-REKSIFFR       TO SU2-REKSIFFR-ERS                          
280000                                                                          
290042     MOVE FOR-KDRESTR        TO SU2-KDRESTR                               
291042     IF FOR-KDSTARAD = 1 AND FOR-FLTPOBEK = NEJ                           
300042        MOVE ZERO            TO SU2-KDRO                                  
300112     ELSE                                                                 
301042        MOVE 2               TO SU2-KDRO                                  
302012     END-IF                                                               
310042     MOVE FOR-KDORDKL        TO SU2-KDORDER                               
320042     MOVE 1                  TO SU2-FLVRERS                               
330042     MOVE FOR-KDERS          TO SU2-KDERS                                 
340042     MOVE FOR-KDORDKL        TO SU2-KDORDKL                               
350042     MOVE FOR-KDFAKTYP       TO SU2-KDFAKTYP                              
360043     MOVE ZERO               TO SU2-IDORDNR7-LEV                          
370042     MOVE FOR-KDTPOTYP       TO SU2-KDTPOTYP                              
380000                                                                          
390042     PERFORM S40-RED-KDVRTPO-SU                                           
400042     MOVE KDVRTPO-SU-WS      TO SU2-KDVRTPO                               
410000                                                                          
420042     MOVE FOR-KDVRINFO       TO SU2-KDVRINFO                              
430000                                                                          
440042     MOVE TIAAMMDD           TO SU2-TIAAMMDD                              
450042     MOVE TIKLOCK            TO SU2-TIKLOCK                               
460000     .                                                                    
470000     EJECT                                                                
480042 S31-SU2-RED-TIL-DEL-SKR SECTION.                                         
490000*                                                                         
500042*    SU2 - REDIGERA TILLK-DEL OCH SKRIV                                   
510000*                                                                         
520042     MOVE FOR-IDARTNR        TO SU2-IDARTNR-TILLK                         
530042     MOVE FOR-REKSIFFR       TO SU2-REKSIFFR-TILLK                        
540000                                                                          
550042     IF  FOR-DIERS-ERS > ZERO                                             
560042       COMPUTE SU2-DIERS     = FOR-DIERS-TILLK                            
570042                             / FOR-DIERS-ERS                              
580042         ON SIZE ERROR MOVE ZERO TO SU2-DIERS                             
590042       END-COMPUTE                                                        
600000     ELSE                                                                 
610042       MOVE ZERO             TO SU2-DIERS                                 
620000     END-IF                                                               
630000                                                                          
640042     WRITE PSU2 FROM SU2-W425SU2-CTX                                      
650042     PERFORM S01-UTPOST-VR                                                
660000     .                                                                    
670000     EJECT                                                                
680042 S32-SU2-INIT-TIL-DEL-SKR SECTION.                                        
690000*                                                                         
700042*    SU2 - INITIERA (NOLL/BLANK) TILLK-DEL OCH SKRIV                      
710000*                                                                         
720042     MOVE ZERO               TO SU2-IDARTNR-TILLK                         
730042     MOVE ZERO               TO SU2-REKSIFFR-TILLK                        
740000                                                                          
750042     MOVE ZERO               TO SU2-DIERS                                 
760000                                                                          
770045     MOVE ZERO               TO SU2-KVLEVART-002                          
780000                                                                          
790042     WRITE PSU2 FROM SU2-W425SU2-CTX                                      
800042     PERFORM S01-UTPOST-VR                                                
810000     .                                                                    
820000     EJECT                                                                
830042 S33-SU3-RED-SKR SECTION.                                                 
840000*                                                                         
850042*    SU3 - REDIGERA OCH SKRIV                                             
860044*    FÄLTEN IDORDNR-002 OCH KVBEART REDIGERAS                             
870042*    UTANFÖR DENNA SECTION.                                               
880000*                                                                         
890042     MOVE 'SU3'              TO SU3-IDPTYP                                
900000                                                                          
940042     MOVE FOR-IDDISTR        TO SU3-IDDISTR                               
941042     MOVE FOR-IDKUNDNR       TO SU3-IDKUNDNR                              
950042     MOVE 711                TO SU3-IDSUPPL                               
960042     MOVE FOR-IDARTNR        TO SU3-IDARTNR                               
970042     MOVE FOR-REKSIFFR       TO SU3-REKSIFFR                              
980042     MOVE 2                  TO SU3-KDKVFOR                               
990042     MOVE FOR-KDRESTR        TO SU3-KDRESTR                               
000042     MOVE 2                  TO SU3-KDRO                                  
010042     MOVE FOR-KDORDKL        TO SU3-KDORDER                               
020043     MOVE ZERO               TO SU3-IDORDNR7-LEV                          
030042     MOVE FOR-KVQPACK-1      TO SU3-KVQPACK-1                             
040000                                                                          
050042     IF  FOR-KDRADERS = +1                                                
060042       MOVE JA               TO SU3-TID-ERS                               
070000     ELSE                                                                 
080042       MOVE NEJ              TO SU3-TID-ERS                               
090000     END-IF                                                               
100000                                                                          
110042     MOVE FOR-KDORDKL        TO SU3-KDORDKL                               
120042     MOVE FOR-KDFAKTYP       TO SU3-KDFAKTYP                              
130042     MOVE ZERO               TO SU3-FIKTIV-KVANT                          
140000                                                                          
150042     PERFORM S40-RED-KDVRTPO-SU                                           
160042     MOVE KDVRTPO-SU-WS      TO SU3-KDVRTPO                               
170000                                                                          
180000                                                                          
190042     MOVE FOR-KDVRINFO       TO SU3-KDVRINFO                              
200000                                                                          
210042     MOVE TIAAMMDD           TO SU3-TIAAMMDD                              
220042     MOVE TIKLOCK            TO SU3-TIKLOCK                               
230000                                                                          
240042     WRITE PSU3 FROM SU3-W425SU3-CTX                                      
250042     PERFORM S01-UTPOST-VR                                                
260000     .                                                                    
270000     EJECT                                                                
280042 S34-SU5-RED-SKR SECTION.                                                 
290000*                                                                         
300042*    SU5 - REDIGERA OCH SKRIV                                             
310000*                                                                         
320042     MOVE 'SU5'              TO SU5-IDPTYP                                
330000                                                                          
370042     MOVE FOR-IDDISTR        TO SU5-IDDISTR                               
371042     MOVE FOR-IDKUNDNR       TO SU5-IDKUNDNR                              
380042     MOVE 711                TO SU5-IDSUPPL                               
390042     MOVE IDORDNR-WS         TO SU5-IDRONR                                
400042     MOVE FOR-IDARTNR        TO SU5-IDARTNR                               
410042     MOVE FOR-REKSIFFR       TO SU5-REKSIFFR                              
420042     MOVE FOR-KVART          TO SU5-KVRO                                  
430000                                                                          
440042     MOVE FOR-KDORDKL        TO SU5-KDORDER                               
450042     MOVE FOR-KDFAKTYP       TO SU5-KDFAKTYP                              
460042     MOVE FOR-KDORDKL        TO SU5-KDORDKL                               
470042     MOVE FOR-KDVRINFO       TO SU5-KDVRINFO                              
480000                                                                          
490042     MOVE TIAAMMDD           TO SU5-TIAAMMDD-REG                          
500042     MOVE TIKLOCK            TO SU5-TIKLOCK-REG                           
510000                                                                          
520042     WRITE PSU5 FROM SU5-W425SU5                                          
530042     PERFORM S01-UTPOST-VR                                                
540000     .                                                                    
550000     EJECT                                                                
560042 S35-SUD-RED-SKR SECTION.                                                 
570000*                                                                         
580042*    SUD - REDIGERA OCH SKRIV                                             
590042*    FÄLTET KVBEART REDIGERAS UTANFÖR DENNA SECTION.                      
600000*                                                                         
610042     MOVE 'SUD'              TO SUD-IDPTYP                                
620000                                                                          
660042     MOVE FOR-IDDISTR        TO SUD-IDDISTR                               
661042     MOVE FOR-IDKUNDNR       TO SUD-IDKUNDNR                              
670042     MOVE 711                TO SUD-IDSUPPL                               
680044     MOVE IDORDNR-WS         TO SUD-IDORDNR-002                           
690042     MOVE FOR-IDARTNR        TO SUD-IDARTNR                               
700042     MOVE FOR-REKSIFFR       TO SUD-REKSIFFR                              
710042     MOVE FOR-TITPO          TO SUD-TITPO                                 
720042     MOVE FOR-KDTPOTYP       TO SUD-KDTPOTYP                              
730000                                                                          
740042     PERFORM S40-RED-KDVRTPO-SU                                           
750042     MOVE KDVRTPO-SU-WS      TO SUD-KDVRTPO                               
760000                                                                          
770042     MOVE FOR-KDVRINFO       TO SUD-KDVRINFO                              
780000                                                                          
790042     MOVE TIAAMMDD           TO SUD-TIAAMMDD                              
800042     MOVE TIKLOCK            TO SUD-TIKLOCK                               
810042     WRITE PSUD FROM SUD-W425SUD-CTX                                      
820042     PERFORM S01-UTPOST-VR                                                
830000     .                                                                    
840000     EJECT                                                                
200042 S40-RED-KDVRTPO-SU SECTION.                                              
210000*                                                                         
220042*    REDIGERA KDVRTPO FÖR SU-POSTER                                       
230000*                                                                         
240042     IF  FOR-KDSTARAD = '1'                                               
250042     AND FOR-KDTPOTYP = 1                                                 
260042       IF  FOR-IDSYSTEM = 'VR'                                            
270042         MOVE 1              TO KDVRTPO-SU-WS                             
280000       ELSE                                                               
290042         MOVE 2              TO KDVRTPO-SU-WS                             
300000       END-IF                                                             
310000     ELSE                                                                 
320042       MOVE ZERO             TO KDVRTPO-SU-WS                             
330000     END-IF                                                               
340000     .                                                                    
