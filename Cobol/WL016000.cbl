120000 ID DIVISION.                                                             
130000 PROGRAM-ID.     WL016000.                                                
140000 AUTHOR.         SUBBARAO PARUCHURI V.                                    
150000 DATE-WRITTEN.   04/09/28.                                                
160000 DATE-COMPILED.                                                           
170000                                                                          
180000*    NAME:       'CARPARTS.LDC.REMAININGDISCRLINES'                       
190000*                                                                         
190100*                                                                         
200000*    FUNCTION:                                                            
210000*        VISAR KÖ MED OBEHANDLADE LEVERANSANMÄRKNINGAR                    
220000*                                                                         
230000*        WL016000 PROGRAM IS A REPLICA OF W4072100 PROGRAM                
231000*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
240000*                                                                         
250000*    INDATA.                                                              
260000*        TRANSACTION: WL0160T                                             
270000*        REQUEST:     WL0160I1                                            
280000*                                                                         
290000*    OUTDATA.                                                             
300000*        RESPONSE:    WL0160O1                                            
310000*                                                                         
310000*    ETRACKER 10296404 - RETURNS FROM CA TO US                            
310000*    ETRACKER 10302968 - GENERIC SOLUTION IDFTG                           
  0000*                                                                         
320000     SKIP3                                                                
330000 ENVIRONMENT DIVISION.                                                    
340000     SKIP2                                                                
350000 INPUT-OUTPUT SECTION.                                                    
360000                                                                          
370000 FILE-CONTROL.                                                            
400000     EJECT                                                                
410000 DATA DIVISION.                                                           
420000     SKIP3                                                                
430000 FILE SECTION.                                                            
450000     EJECT                                                                
460000 WORKING-STORAGE SECTION.                                                 
470000 77  IDPGM                       PIC X(08)   VALUE 'WL016000'.            
480000                                                                          
490000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
500000 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
510000 77  KDRC-DISPLAY                PIC Z(5).                                
520000                                                                          
530000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
540000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
550000                                                                          
560000 77  JA                          PIC X       VALUE 'J'.                   
570000 77  YES                         PIC X       VALUE 'Y'.                   
580000 77  NEJ                         PIC X       VALUE 'N'.                   
590000                                                                          
600000*    --- INDEX FÖR BLÄDDRINGSRADER                                        
610000 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
620000 77  MAX-INDX                    PIC S9(4)  VALUE +500  COMP SYNC.        
649900                                                                          
650600 77  WS-IDELMT-ERROR             PIC X(16).                               
650700 77  WS-IDMSG-ERROR              PIC X(03).                               
650800 77  WS-IDMSG-INFO               PIC X(03).                               
651000     EJECT                                                                
660000*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
670000 01  GENERAL-SUBPROGRAMS.                                                 
680000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
690000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
700000     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
710000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
720000     SKIP3                                                                
730000*    --- PARAMETERS TO ABEND                                              
740000                                                                          
750000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
760000 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
770000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
870000     EJECT                                                                
880000*                                                                         
890000 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
900000     SKIP3                                                                
910000*01  -COPY WZ01SUB                                                        
920000     EJECT                                                                
930000 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
940000     SKIP3                                                                
950000 01  REQU-AREA.                                                           
960000*    03  -COPY WZ01REQU                                                   
970000*    03  -COPY WL0160I1                                                   
980000     EJECT                                                                
990000 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
000000     SKIP3                                                                
010000 01  RESP-AREA.                                                           
020000*    03  -COPY WZ01RESP                                                   
030000*    03  -COPY WL0160O1                                                   
040000     EJECT                                                                
050000 01  MESSAGE-CODES.                                                       
052000     03  ERR-INFO-MISSING        PIC X(3)    VALUE '185'.                 
058100     03  SYS-ERR                 PIC  X(03)  VALUE '099'.                 
058200     03  NO-LINES-FOUND          PIC  X(03)  VALUE '027'.                 
058300     03  TOO-MANY-LINES          PIC  X(03)  VALUE '028'.                 
059000     EJECT                                                                
059100*    --- FÄLT FÖR HOPP TILL ANDRA BILDER                                  
059200   77  SW-STARTA-ANNAN-BILD        PIC X       VALUE 'N'.                 
059300     88  STARTA-ANNAN-BILD                     VALUE 'J'.                 
059400                                                                          
059500   77  NYCKLAR-SW                  PIC X       VALUE 'J'.                 
059600     88  NYCKLAR-OK                            VALUE 'J'.                 
059700     88  NYCKLAR-FEL                           VALUE 'N'.                 
059800                                                                          
061800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
061900*                                                                         
062000                                                                          
062100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
062200     SKIP3                                                                
063600 01  NYCKLAR-TILL-DLI.                                                    
063700                                                                          
063800     03  W-WDA2B1KY-MIN-X.                                                
063900         05  W-IDFTG-B1-MIN      PIC  9(2)          VALUE ZERO.           
064000         05  W-KDARBTYP-B1-MIN   PIC  X(8)          VALUE SPACE.          
064100         05  W-IDPERSON-B1-MIN   PIC S9(3)   COMP-3 VALUE ZERO.           
064200         05  W-DALEVANM-B1-MIN   PIC  9(8)          VALUE ZERO.           
064300         05  W-IDDISTR-B1-MIN    PIC S9(5)   COMP-3 VALUE ZERO.           
064400         05  W-IDKUNDNR-B1-MIN   PIC S9(7)   COMP-3 VALUE ZERO.           
064500         05  W-IDRAPPNR-B1-MIN   PIC  X(7)          VALUE ZERO.           
064600         05  W-IDARTNR-B1-MIN    PIC S9(9)   COMP-3 VALUE ZERO.           
064700         05  W-IDRADNR-B1-MIN    PIC S9(5)   COMP-3 VALUE ZERO.           
064800                                                                          
064900     03  W-WDA2B1KY-MAX-X.                                                
065000         05  W-IDFTG-B1-MAX      PIC  9(2)          VALUE ZERO.           
065100         05  W-KDARBTYP-B1-MAX   PIC  X(8)          VALUE SPACE.          
065200         05  W-IDPERSON-B1-MAX   PIC S9(3)   COMP-3 VALUE ZERO.           
065300         05  W-DALEVANM-B1-MAX   PIC  9(8)          VALUE ZERO.           
065400         05  W-IDDISTR-B1-MAX    PIC S9(5)   COMP-3 VALUE ZERO.           
065500         05  W-IDKUNDNR-B1-MAX   PIC S9(7)   COMP-3 VALUE ZERO.           
065600         05  W-IDRAPPNR-B1-MAX   PIC  X(7)          VALUE ZERO.           
065700         05  W-IDARTNR-B1-MAX    PIC S9(9)   COMP-3 VALUE ZERO.           
065800         05  W-IDRADNR-B1-MAX    PIC S9(5)   COMP-3 VALUE ZERO.           
065900                                                                          
067000     03  W-IDDC-B6-X.                                                     
067100         05 W-IDDC-B6            PIC X(2).                                
067200                                                                          
067800     SKIP2                                                                
067900*    --- STATUS-KOD FRÅN IMS                                              
068000 01  STATUS-WS                   PIC XX.                                  
068100     88  STATUS-OK                           VALUE '  '.                  
068200     88  SEGMENT-FINNS                       VALUE '  '.                  
068300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
068400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
068500     88  TRANSKOD-FEL                        VALUE 'A1'.                  
068600     88  SECURITY-FEL                        VALUE 'A4'.                  
068700     SKIP2                                                                
068800 01  GODK-STATUSKODER.                                                    
068900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
069000     SKIP3                                                                
069100 01  SSA1                        PIC X(250).                              
069200     EJECT                                                                
069300*    --- IMS FUNKTIONSKODER                                               
069400*01  -COPY W0003                                                          
069500     EJECT                                                                
069600*    ---  DLI INPUT-OUTPUT AREA                                           
069700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
069800     SKIP3                                                                
069900 01  DLI-IO-AREA.                                                         
070000     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
070100     SKIP3                                                                
070200     03  WLKREG01 REDEFINES IO-AREA.                                      
070300*        05  -COPY WDA2B1                                                 
080000                                                                          
090000 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
100000 01   DLI-IO-AREA-B601.                                                   
101000*     03  -COPY WDB601                                                    
104000                                                                          
110000     EJECT                                                                
120000 LINKAGE SECTION.                                                         
130000 01  MSG-PCB                     PIC X.                                   
140000     EJECT                                                                
141000*01  -COPY W0008   -PRE KREG-                                             
142000     05  FILLER                  PIC X.                                   
150000     EJECT                                                                
160000*01  -COPY W0008   -PRE WDB6-                                             
160100     05  FILLER                  PIC X.                                   
160200     EJECT                                                                
160300 PROCEDURE DIVISION  USING MSG-PCB KREG-PCB WDB6-PCB.                     
160400 MAIN SECTION.                                                            
161000     ENTRY 'DLITCBL' USING MSG-PCB KREG-PCB WDB6-PCB.                     
170000                                                                          
190000     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
200000     IF SUB-KDRC = 0                                                      
210000       IF REQU-KDPGMACT          = 'S'                                    
211000         PERFORM A-INIT                                                   
212000         PERFORM B-KOLLA-NYCKLAR                                          
213000         IF NYCKLAR-OK                                                    
219700           PERFORM F-LAES-VISA-INFO                                       
219800         END-IF                                                           
221000       ELSE                                                               
230000          MOVE SYS-ERR      TO RESP-IDMSG-ERROR                           
240000       END-IF                                                             
241000                                                                          
250000       MOVE RESP-IDMSG-INFO    TO WS-IDMSG-INFO                           
260000       MOVE RESP-IDMSG-ERROR   TO WS-IDMSG-ERROR                          
270000       MOVE RESP-IDELMT-ERROR  TO WS-IDELMT-ERROR                         
280000       IF WS-IDMSG-ERROR NOT = SPACE                                      
281000           MOVE ALL '+' TO RESP-WL0160O1(1:20)                            
283000           MOVE WS-IDMSG-ERROR   TO RESP-IDMSG-ERROR                      
284000           MOVE WS-IDELMT-ERROR  TO RESP-IDELMT-ERROR                     
285000           MOVE WS-IDMSG-INFO    TO RESP-IDMSG-INFO                       
286000           MOVE 001              TO RESP-IDMSGVER                         
289100            MOVE ZERO             TO RESP-KVRADER                         
289300       END-IF                                                             
289400                                                                          
290000       PERFORM S02-RETURN-RESPONSE                                        
300000     END-IF                                                               
320000                                                                          
330000                                                                          
350000     MOVE ZERO TO RETURN-CODE                                             
360000     GOBACK                                                               
370000     .                                                                    
380000     EJECT                                                                
390000 A-INIT SECTION.                                                          
400000                                                                          
670000     MOVE LOW-VALUE         TO W-WDA2B1KY-MIN-X                           
710000                                                                          
720000     MOVE HIGH-VALUE        TO W-WDA2B1KY-MAX-X                           
750100                                                                          
751000     MOVE ALL '+'           TO RESP-AREA                                  
752000     MOVE SPACE             TO RESP-IDMSG-ERROR                           
753000                               RESP-IDMSG-INFO                            
754000                               RESP-IDELMT-ERROR                          
755000     MOVE 001               TO RESP-IDMSGVER                              
756000     MOVE ZERO              TO RESP-KVRADER                               
760000                                                                          
760100     MOVE REQU-IDDC-KEY     TO W-IDDC-B6                                  
761000     PERFORM IMS-GU-WDB601                                                
770000     .                                                                    
780000     EJECT                                                                
911400 B-KOLLA-NYCKLAR  SECTION.                                                
911500                                                                          
911600     MOVE JA                TO NYCKLAR-SW                                 
911700     IF REQU-IDFTG-KEY NOT NUMERIC                                        
911800       MOVE NEJ             TO NYCKLAR-SW                                 
911900       MOVE '123'           TO RESP-IDMSG-ERROR                           
912000       MOVE 'IDFTG'         TO RESP-IDELMT-ERROR                          
912100     END-IF                                                               
912200     .                                                                    
912300     EJECT                                                                
912400                                                                          
912500 F-LAES-VISA-INFO SECTION.                                                
912600                                                                          
912700     PERFORM FA-FIXA-ENTER-KEY                                            
912800     PERFORM IMS-GU-WLKREG01                                              
912900                                                                          
913000     MOVE +1                  TO INDX                                     
913900     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
914000                   INDX > MAX-INDX                                        
914100                                                                          
914300        PERFORM FB-REDIGERA-MOD                                           
914400        PERFORM IMS-GN-WLKREG01                                           
914500        ADD +1  TO INDX                                                   
914600     END-PERFORM                                                          
914700                                                                          
914800     IF SEGMENT-FINNS AND                                                 
914900        INDX > MAX-INDX                                                   
915000                                                                          
915100        MOVE TOO-MANY-LINES       TO RESP-IDMSG-ERROR                     
915200     ELSE                                                                 
915300        IF INDX = 1                                                       
915400           MOVE NO-LINES-FOUND    TO RESP-IDMSG-ERROR                     
915500        END-IF                                                            
916000     END-IF                                                               
916100     .                                                                    
916200     EJECT                                                                
916300                                                                          
916400 FA-FIXA-ENTER-KEY        SECTION.                                        
916500                                                                          
916500*    FIX TO MAKE IT POSSIBLE FOR A SPECIFIC USER TO HANDLE                
916500*    RETURNS FROM CA (FTG=54) TO US (DC=44, FTG=53)                       
917800*    IF REQU-IDUSER = 'PHCA4G1'                                           
917800*       AND DCS-IDDC = '44'                                               
917800*      MOVE '54'                 TO W-IDFTG-B1-MIN                        
917900*                                   W-IDFTG-B1-MAX                        
919000*      MOVE '440'                TO W-IDPERSON-B1-MIN                     
919100*                                   W-IDPERSON-B1-MAX                     
      * END FIX                                                                 
      *    ELSE                                                                 
917800     MOVE REQU-IDFTG-KEY       TO W-IDFTG-B1-MIN                          
917900                                  W-IDFTG-B1-MAX                          
919000     MOVE DCS-IDPERSON-REM     TO W-IDPERSON-B1-MIN                       
919100                                  W-IDPERSON-B1-MAX                       
      *    END-IF                                                               
918000     MOVE 'REM'                  TO W-KDARBTYP-B1-MIN                     
918100                                    W-KDARBTYP-B1-MAX                     
919400     .                                                                    
919500     EJECT                                                                
919600                                                                          
919700 FB-REDIGERA-MOD         SECTION.                                         
919800                                                                          
920100     MOVE SEQB-KDARBTYP      TO RESP-IDANSV  (INDX) (1:3)                 
920200     MOVE SEQB-IDPERSON      TO RESP-IDANSV  (INDX) (4:3)                 
920300     MOVE SEQB-DALEVANM(3:6) TO RESP-TILEVANM (INDX)                      
920400                                                                          
920500     MOVE SEQB-IDDISTR       TO RESP-IDDISTR (INDX)                       
920600     MOVE SEQB-IDKUNDNR      TO RESP-IDKUNDNR (INDX)                      
920700     MOVE SEQB-IDRAPPNR      TO RESP-IDRAPPNR (INDX)                      
920800     MOVE SEQB-IDORDNR7      TO RESP-IDORDNR5 (INDX)                      
920900     MOVE SEQB-IDRADNR       TO RESP-IDRADNR  (INDX)                      
921000     MOVE SEQB-IDARTNR       TO RESP-IDARTNR  (INDX)                      
921100     MOVE SEQB-KVLEVANM-BEKR TO RESP-KVLEVANM-BEKR (INDX)                 
921200     MOVE SEQB-KDANMORS      TO RESP-KDANMORS (INDX)                      
921300     MOVE SEQB-FLTEXT        TO RESP-FLTEXT   (INDX)                      
921400     IF SEQB-FLTEXT           = JA                                        
921500         MOVE YES               TO RESP-FLTEXT  (INDX)                    
921600     ELSE                                                                 
921700       MOVE SEQB-FLTEXT         TO RESP-FLTEXT  (INDX)                    
921800     END-IF                                                               
921900     MOVE SEQB-KDKREBEH      TO RESP-KDKREBEH (INDX)                      
922700     MOVE INDX               TO RESP-KVRADER                              
922800     .                                                                    
922900     EJECT                                                                
924900                                                                          
929200                                                                          
929300*    --- DISPATCHER SECTIONS                                              
930000 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
940000                                                                          
950000     MOVE 'GETARG'               TO SUB-KDFUNC                            
960000     MOVE 'CARPARTS.LDC.REMAININGDISCRLINES' TO SUB-ADDISPABS             
970000     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
980000                                                                          
990000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
000000                                                                          
010000     IF SUB-KDRC > 0                                                      
020000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
030000       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
040000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
050000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
060000     END-IF                                                               
070000     .                                                                    
080000     SKIP3                                                                
090000 S02-RETURN-RESPONSE SECTION.                                             
100000                                                                          
110000     MOVE 'RETURN'                   TO SUB-KDFUNC                        
120000     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
130000                                                                          
140000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
150000                                                                          
160000     IF SUB-KDRC > 0                                                      
170000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
180000       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
190000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
200000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
210000     END-IF                                                               
220000     .                                                                    
230000     EJECT                                                                
240000* --- IMS SEKTIONER ---                                                   
241000     SKIP3                                                                
250000 IMS-GU-WLKREG01       SECTION.                                           
260000                                                                          
270000     STRING 'WLKREG01(WDA2B1KY>=' W-WDA2B1KY-MIN-X                        
280000                    '&WDA2B1KY<=' W-WDA2B1KY-MAX-X ')'                    
350000          DELIMITED BY SIZE INTO SSA1                                     
360000     MOVE '  GE'           TO GODK-STATUSKODER                            
370000     CALL CBLTDLI USING GU KREG-PCB DLI-IO-AREA SSA1                      
380000     MOVE KREG-STATUS-CODE TO STATUS-WS                                   
390000     PERFORM IMS-STATUSKONTROLL                                           
400000     .                                                                    
410000                                                                          
420000 IMS-GN-WLKREG01       SECTION.                                           
430000                                                                          
440000     STRING 'WLKREG01(WDA2B1KY>=' W-WDA2B1KY-MIN-X                        
450000                    '&WDA2B1KY<=' W-WDA2B1KY-MAX-X ')'                    
520000          DELIMITED BY SIZE INTO SSA1                                     
530000     MOVE '  GEGB'           TO GODK-STATUSKODER                          
540000     CALL CBLTDLI USING GN KREG-PCB DLI-IO-AREA SSA1                      
550000     MOVE KREG-STATUS-CODE TO STATUS-WS                                   
560000     PERFORM IMS-STATUSKONTROLL                                           
570000     .                                                                    
580000     EJECT                                                                
590000                                                                          
591000 IMS-GU-WDB601    SECTION.                                                
591100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
591200          DELIMITED BY SIZE INTO SSA1                                     
591300     MOVE '  GE' TO GODK-STATUSKODER                                      
591400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
592000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
593000     PERFORM IMS-STATUSKONTROLL                                           
594000     IF SEGMENT-SAKNAS                                                    
595000         MOVE SPACE TO DCS-KDDC                                           
595100         MOVE ZERO  TO DCS-IDPERSON-REM                                   
596000     END-IF                                                               
597000     .                                                                    
598000                                                                          
610000 IMS-STATUSKONTROLL SECTION.                                              
620000                                                                          
630000     SET STATUS-IX TO 1                                                   
640000     SEARCH GODK-STATUS                                                   
650000       AT END                                                             
660000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
670000         DELIMITED BY SIZE INTO FELTEXT                                   
680000         CALL FELLOG                                                      
690000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
700000         CONTINUE                                                         
710000     END-SEARCH                                                           
720000     .                                                                    
