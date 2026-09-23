120000 ID DIVISION.                                                             
130000 PROGRAM-ID.     WL016200.                                                
140000 AUTHOR.         SUBBARAO PARUCHURI V.                                    
150000 DATE-WRITTEN.   04/10/11.                                                
160000 DATE-COMPILED.                                                           
170000                                                                          
180000*    NAME:       'CARPARTS.LDC.DISCRLINETEXT'                             
190000*                                                                         
190100*                                                                         
200000*    FUNCTION:                                                            
210000*        VISA/UPPDATERA TEXT INFO PÅ LEVERANSANMÄRKNINGSRAD               
220000*        PROGRAMMET UPPDATERAR WLKREE (WDA2)                              
230000*                                                                         
240000*        WL016200 PROGRAM IS A REPLICA OF W4072300 PROGRAM                
241000*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
250000*                                                                         
260000*    INDATA.                                                              
270000*        TRANSACTION: WL0162U                                             
280000*        REQUEST:     WL0162I1                                            
290000*                                                                         
300000*    OUTDATA.                                                             
310000*        RESPONSE:    WL0162O1                                            
320000*                                                                         
      *    ETRACKER = 10296404 RETURNS FROM CA TO US                            
      *    ETRACKER = 10302968 GENERIC SOLUTION IDFTG                           
      *                                                                         
330000     SKIP3                                                                
340000 ENVIRONMENT DIVISION.                                                    
350000     SKIP2                                                                
360000 INPUT-OUTPUT SECTION.                                                    
370000                                                                          
380000 FILE-CONTROL.                                                            
410000     EJECT                                                                
420000 DATA DIVISION.                                                           
430000     SKIP3                                                                
440000 FILE SECTION.                                                            
460000     EJECT                                                                
470000 WORKING-STORAGE SECTION.                                                 
480000 77  IDPGM                       PIC X(08)   VALUE 'WL016200'.            
490000                                                                          
500000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
510000 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
520000 77  KDRC-DISPLAY                PIC Z(5).                                
530000                                                                          
540000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
550000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
560000                                                                          
570000 77  JA                          PIC X       VALUE 'J'.                   
580000 77  YES                         PIC X       VALUE 'Y'.                   
590000 77  NEJ                         PIC X       VALUE 'N'.                   
600000                                                                          
610000 77  INDX                        PIC S9(4)   VALUE +0  COMP SYNC.         
620000                                                                          
630000                                                                          
640000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
650000     88  INDATA-OK                           VALUE 'J'.                   
651000     88  INDATA-FEL                          VALUE 'N'.                   
652000                                                                          
653000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
654000     88  NYCKLAR-OK                          VALUE 'J'.                   
655000     88  NYCKLAR-FEL                         VALUE 'N'.                   
659300                                                                          
659400 77  OK-BEHANDLAD                PIC X(3)    VALUE '101'.                 
659500 77  STARTAD-AV-DISPATCHEN-SW    PIC X       VALUE 'N'.                   
659600     88  STARTAD-AV-DISPATCHEN               VALUE 'J'.                   
659700                                                                          
659800                                                                          
659900 77  WS-IDELMT-ERROR             PIC X(16).                               
660000 77  WS-IDMSG-ERROR              PIC X(03).                               
660100 77  WS-IDMSG-INFO               PIC X(03).                               
660200                                                                          
661000     EJECT                                                                
670000*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
680000 01  GENERAL-SUBPROGRAMS.                                                 
690000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
700000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
710000     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
720000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
730000     SKIP3                                                                
731000 01  MESSAGE-CODES.                                                       
735000     03  ERR-INFO-MISSING        PIC X(3)    VALUE '025'.                 
737000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '001'.                 
738100     03  SYS-ERROR               PIC X(3)    VALUE '099'.                 
738200     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '00A'.                 
739000     EJECT                                                                
740000*    --- PARAMETERS TO ABEND                                              
750000                                                                          
760000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
770000 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
780000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
880000     EJECT                                                                
890000*                                                                         
900000 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
910000     SKIP3                                                                
920000*01  -COPY WZ01SUB                                                        
930000     EJECT                                                                
940000 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
950000     SKIP3                                                                
960000 01  REQU-AREA.                                                           
970000*    03  -COPY WZ01REQU                                                   
980000*    03  -COPY WL0162I1                                                   
990000     EJECT                                                                
000000 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
010000     SKIP3                                                                
020000 01  RESP-AREA.                                                           
030000*    03  -COPY WZ01RESP                                                   
040000*    03  -COPY WL0162O1                                                   
050000     EJECT                                                                
070000     EJECT                                                                
080000                                                                          
120000     EJECT                                                                
121000     SKIP2                                                                
122000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
123000*                                                                         
124000     EJECT                                                                
125000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
126000     SKIP3                                                                
127000                                                                          
128000 01  NYCKLAR-TILL-DLI.                                                    
129000                                                                          
129100     03  W-IDLEVANM-X.                                                    
129200         05  W-IDDISTR           PIC S9(5)   COMP-3 VALUE ZERO.           
129300         05  W-IDKUNDNR          PIC S9(7)   COMP-3 VALUE ZERO.           
129400         05  W-IDRAPPNR          PIC  X(7)          VALUE ZERO.           
129500                                                                          
129600     03  W-WDA211KY-X.                                                    
129700         05  W-IDARTNR           PIC S9(9)   COMP-3 VALUE ZERO.           
129800         05  W-IDRADNR           PIC S9(5)   COMP-3 VALUE ZERO.           
129900                                                                          
130000     03  W-KDSEGKEY-X.                                                    
130100         05  W-KDSEGKEY          PIC  X(1)          VALUE '1'.            
130200     SKIP2                                                                
130300*    --- STATUS-KOD FRÅN IMS                                              
130400 01  STATUS-WS                   PIC XX.                                  
130500     88  STATUS-OK                           VALUE '  '.                  
130600     88  SEGMENT-FINNS                       VALUE '  '.                  
130700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
130800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
130900     SKIP2                                                                
131000 01  GODK-STATUSKODER.                                                    
131100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
131200     SKIP3                                                                
131300 01  SSA1                        PIC X(192).                              
131400 01  SSA2                        PIC X(64).                               
131500 01  SSA3                        PIC X(64).                               
131600     EJECT                                                                
131700*    --- IMS FUNKTIONSKODER                                               
131800*01  -COPY W0003                                                          
131900     EJECT                                                                
132000*    ---  DLI INPUT-OUTPUT AREA                                           
132100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
132200     SKIP3                                                                
132300 01  DLI-IO-AREA.                                                         
132400     03  IO-AREA                 PIC X(1100) VALUE SPACE.                 
132500     03  WLKREE11 REDEFINES IO-AREA.                                      
132600*        05  -COPY WDA211                                                 
132700     EJECT                                                                
132800     03  WLKREE21 REDEFINES IO-AREA.                                      
132900*        05  -COPY WDA221                                                 
133000                                                                          
133100 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDA201'.         
133200 01  DLI-IO-WDA201.                                                       
133300*    03  -COPY WDA201                                                     
133400     EJECT                                                                
134000 LINKAGE SECTION.                                                         
140000 01  MSG-PCB                     PIC X.                                   
160000     EJECT                                                                
170100*01  -COPY W0008   -PRE KREE-                                             
170200     05  FILLER                  PIC X.                                   
170300     EJECT                                                                
170400 PROCEDURE DIVISION  USING MSG-PCB KREE-PCB.                              
170500 MAIN SECTION.                                                            
171000     ENTRY 'DLITCBL' USING MSG-PCB KREE-PCB.                              
180000                                                                          
200000     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
210000     IF SUB-KDRC = 0                                                      
220000      IF REQU-KDPGMACT = 'E' OR 'S'                                       
225000         PERFORM A-INIT                                                   
226000         PERFORM B-KOLLA-NYCKLAR                                          
227000         IF NYCKLAR-OK                                                    
228100           IF REQU-KDPGMACT = 'E'                                         
229000               PERFORM H-UPPDATERA                                        
229800           END-IF                                                         
230000              PERFORM F-LAES-VISA-INFO                                    
230200         END-IF                                                           
230900      ELSE                                                                
231000       MOVE SYS-ERROR    TO RESP-IDMSG-ERROR                              
240000      END-IF                                                              
241000                                                                          
250000       MOVE RESP-IDMSG-INFO    TO WS-IDMSG-INFO                           
260000       MOVE RESP-IDMSG-ERROR   TO WS-IDMSG-ERROR                          
270000       MOVE RESP-IDELMT-ERROR  TO WS-IDELMT-ERROR                         
280000       IF WS-IDMSG-ERROR NOT = SPACE                                      
290000           MOVE ALL '+' TO RESP-WL0162O1(1:31)                            
292000           MOVE WS-IDMSG-ERROR   TO RESP-IDMSG-ERROR                      
293000           MOVE WS-IDELMT-ERROR  TO RESP-IDELMT-ERROR                     
294000           MOVE WS-IDMSG-INFO    TO RESP-IDMSG-INFO                       
295000           MOVE 001              TO RESP-IDMSGVER                         
296000       END-IF                                                             
297000                                                                          
300000       PERFORM S02-RETURN-RESPONSE                                        
310000     END-IF                                                               
330000                                                                          
340000                                                                          
360000     MOVE ZERO TO RETURN-CODE                                             
370000     GOBACK                                                               
380000     .                                                                    
390000     EJECT                                                                
400000 A-INIT SECTION.                                                          
410000                                                                          
671000     MOVE ALL '+'           TO RESP-AREA                                  
672000     MOVE SPACE             TO RESP-IDMSG-ERROR                           
673000                               RESP-IDMSG-INFO                            
674000                               RESP-IDELMT-ERROR                          
675000     MOVE 001               TO RESP-IDMSGVER                              
680000     .                                                                    
690000     EJECT                                                                
700000 B-KOLLA-NYCKLAR SECTION.                                                 
710000                                                                          
927000                                                                          
928000        MOVE JA TO NYCKLAR-SW                                             
929000                                                                          
929100        PERFORM BA-KOLLA-IDDISTR                                          
929200        PERFORM BB-KOLLA-IDKUNDNR                                         
929300        PERFORM BC-KOLLA-IDRAPPNR                                         
929400        PERFORM BD-KOLLA-IDARTNR                                          
929500        PERFORM BE-KOLLA-IDRADNR                                          
929600                                                                          
929700        IF NYCKLAR-OK                                                     
929800          PERFORM BF-KOLLA-IDFTG-USER                                     
929900        END-IF                                                            
930000                                                                          
930100        MOVE REQU-IDDC-KEY  TO RESP-IDDC-KEY                              
930900     .                                                                    
931000     EJECT                                                                
931100                                                                          
931200                                                                          
931300 BA-KOLLA-IDDISTR  SECTION.                                               
931400                                                                          
932200     IF REQU-IDDISTR-KEY NUMERIC AND REQU-IDDISTR-KEY > ZERO              
932300       MOVE REQU-IDDISTR-KEY    TO W-IDDISTR                              
932400                                   RESP-IDDISTR-KEY                       
932500       INSPECT RESP-IDDISTR-KEY REPLACING LEADING ZERO BY SPACE           
932600     ELSE                                                                 
932700       MOVE '023'               TO RESP-IDMSG-ERROR                       
932800       MOVE 'IDDISTR'           TO RESP-IDELMT-ERROR                      
932900       MOVE NEJ                 TO NYCKLAR-SW                             
933000     END-IF                                                               
933100                                                                          
933200     .                                                                    
933300     EJECT                                                                
933400                                                                          
933500 BB-KOLLA-IDKUNDNR   SECTION.                                             
933600                                                                          
934200     IF REQU-IDKUNDNR-KEY       NUMERIC                                   
934300       MOVE REQU-IDKUNDNR-KEY   TO W-IDKUNDNR                             
934400                                   RESP-IDKUNDNR-KEY                      
934500       INSPECT RESP-IDKUNDNR-KEY REPLACING LEADING ZERO BY SPACE          
934600     ELSE                                                                 
934700       MOVE '023'               TO RESP-IDMSG-ERROR                       
934800       MOVE 'IDKUNDNR'          TO RESP-IDELMT-ERROR                      
934900       MOVE NEJ                 TO NYCKLAR-SW                             
935000     END-IF                                                               
935100                                                                          
935200     .                                                                    
935300     EJECT                                                                
935400 BC-KOLLA-IDRAPPNR   SECTION.                                             
935500                                                                          
936100     IF REQU-IDRAPPNR-KEY  NUMERIC AND REQU-IDRAPPNR-KEY > ZERO           
936200       MOVE REQU-IDRAPPNR-KEY   TO W-IDRAPPNR                             
936900     ELSE                                                                 
937000       MOVE '023'               TO RESP-IDMSG-ERROR                       
937100       MOVE 'IDRAPPNR'          TO RESP-IDELMT-ERROR                      
937200       MOVE NEJ                 TO NYCKLAR-SW                             
937300     END-IF                                                               
937400     MOVE REQU-IDRAPPNR-KEY     TO RESP-IDRAPPNR-KEY(1:)                  
937500     INSPECT RESP-IDRAPPNR-KEY REPLACING LEADING ZERO BY SPACE            
937600                                                                          
937700     .                                                                    
937800     EJECT                                                                
937900 BD-KOLLA-IDARTNR   SECTION.                                              
938000                                                                          
938400     IF REQU-IDARTNR-KEY  NUMERIC AND REQU-IDARTNR-KEY > ZERO             
938500       MOVE REQU-IDARTNR-KEY    TO W-IDARTNR                              
938600                                   RESP-IDARTNR-KEY                       
938700       INSPECT RESP-IDARTNR-KEY REPLACING LEADING ZERO BY SPACE           
938800     ELSE                                                                 
938900       MOVE '023'               TO RESP-IDMSG-ERROR                       
939000       MOVE 'IDARTNR'           TO RESP-IDELMT-ERROR                      
939100       MOVE NEJ                 TO NYCKLAR-SW                             
939300     END-IF                                                               
939400                                                                          
939500     .                                                                    
939600     EJECT                                                                
939700 BE-KOLLA-IDRADNR   SECTION.                                              
939800                                                                          
940700     IF REQU-IDRADNR-KEY        NUMERIC                                   
940800       MOVE REQU-IDRADNR-KEY    TO W-IDRADNR                              
940900                                   RESP-IDRADNR-KEY                       
941000       INSPECT RESP-IDRADNR-KEY REPLACING LEADING ZERO BY SPACE           
941100     ELSE                                                                 
941200       MOVE '023'               TO RESP-IDMSG-ERROR                       
941300       MOVE 'IDRADNR'           TO RESP-IDELMT-ERROR                      
941400       MOVE NEJ                 TO NYCKLAR-SW                             
941600     END-IF                                                               
941700                                                                          
941800     .                                                                    
941900     EJECT                                                                
942000 BF-KOLLA-IDFTG-USER SECTION.                                             
943000                                                                          
916500*    FIX TO MAKE IT POSSIBLE FOR A SPECIFIC USER TO HANDLE                
916500*    RETURNS FROM CA (FTG=54) TO US (DC=44, FTG=53)                       
917800*    IF REQU-IDUSER = 'PHCA4G1'                                           
917800*       AND REQU-IDDC-KEY = '44'                                          
917800*      MOVE '54'           TO REQU-IDFTG-KEY                              
070900*    END-IF                                                               
      *    END FIX                                                              
                                                                                
943100     IF REQU-IDFTG-KEY NOT NUMERIC                                        
943200       MOVE NEJ                 TO NYCKLAR-SW                             
943300       MOVE 'IDFTG'             TO RESP-IDELMT-ERROR                      
943400       MOVE '023'               TO RESP-IDMSG-ERROR                       
943900     ELSE                                                                 
944000       PERFORM IMS-GU-WDA201                                              
944100       IF SEGMENT-FINNS                                                   
944200         IF ANM-IDFTG NOT = REQU-IDFTG-KEY                                
944300           MOVE ERR-NOT-AUTHORIZED  TO RESP-IDMSG-ERROR                   
944600           MOVE NEJ                 TO NYCKLAR-SW                         
944700         END-IF                                                           
944800       END-IF                                                             
944900     END-IF                                                               
945100     .                                                                    
945200     EJECT                                                                
945300 F-LAES-VISA-INFO SECTION.                                                
945400                                                                          
945500     PERFORM IMS-GHU-WLKREE11                                             
945600                                                                          
945700     IF SEGMENT-SAKNAS                                                    
945800        MOVE 'IDLEVANM'         TO RESP-IDELMT-ERROR                      
945900        MOVE '025'              TO RESP-IDMSG-ERROR                       
946000        PERFORM FA-TOEM-BILD                                              
946100     ELSE                                                                 
946200        PERFORM IMS-GU-WLKREE21                                           
946300        PERFORM FB-REDIGERA-BILD                                          
946400     END-IF                                                               
946500     .                                                                    
946600     EJECT                                                                
946700                                                                          
946800 FA-TOEM-BILD          SECTION.                                           
946900                                                                          
947000     MOVE +1                          TO INDX                             
947100     PERFORM UNTIL INDX               >  3                                
947200        MOVE ALL '+'                  TO RESP-TEANMNOT-REG(INDX)          
947300                                         RESP-TEANMNOT-DLR(INDX)          
947400                                         RESP-TEANMNOT-ADM(INDX)          
947500                                         RESP-TEANMNOT-REM(INDX)          
947600                                         RESP-TEANMNOT-RET(INDX)          
947700                                                                          
947800        ADD +1                        TO INDX                             
947900     END-PERFORM                                                          
948000                                                                          
948100     .                                                                    
948200     EJECT                                                                
948300                                                                          
948400 FB-REDIGERA-BILD          SECTION.                                       
948500                                                                          
948600     MOVE +1                          TO INDX                             
948700     PERFORM UNTIL INDX               >  3                                
948800        IF SEGMENT-FINNS                                                  
948900          MOVE TXT-TEANMNOT-REG (INDX)  TO RESP-TEANMNOT-REG(INDX)        
949000          MOVE TXT-TEANMNOT-DLR (INDX)  TO RESP-TEANMNOT-DLR(INDX)        
949100          MOVE TXT-TEANMNOT-ADM (INDX)  TO RESP-TEANMNOT-ADM(INDX)        
949200          MOVE TXT-TEANMNOT-REM (INDX)  TO RESP-TEANMNOT-REM(INDX)        
949300          MOVE TXT-TEANMNOT-RET (INDX)  TO RESP-TEANMNOT-RET(INDX)        
949400        ELSE                                                              
949500          MOVE SPACE                    TO RESP-TEANMNOT-REG(INDX)        
949600                                           RESP-TEANMNOT-DLR(INDX)        
949700                                           RESP-TEANMNOT-ADM(INDX)        
949800                                           RESP-TEANMNOT-REM(INDX)        
949900                                           RESP-TEANMNOT-RET(INDX)        
950000        END-IF                                                            
950100        ADD +1                        TO INDX                             
950200     END-PERFORM                                                          
950300                                                                          
950400     .                                                                    
950500     EJECT                                                                
950600                                                                          
950700 H-UPPDATERA  SECTION.                                                    
950800                                                                          
950900     IF REQU-INPUT(1)                 = SPACE  AND                        
951000        REQU-INPUT(2)                 = SPACE  AND                        
951100        REQU-INPUT(3)                 = SPACE                             
951200        CONTINUE                                                          
951300     ELSE                                                                 
951400       PERFORM IMS-GHU-WLKREE11                                           
951500       IF SEGMENT-FINNS                                                   
951600         IF LEV-FLTEXT                = NEJ                               
951700             MOVE JA                  TO LEV-FLTEXT                       
951800             PERFORM IMS-REPL-WLKREE11                                    
951900         END-IF                                                           
952000                                                                          
952100         PERFORM IMS-GHNP-WLKREE21                                        
952200                                                                          
952300         MOVE +1                      TO INDX                             
952400         PERFORM UNTIL INDX           >  3                                
952500           IF REQU-TEANMNOT-REG (INDX) = ALL '+'                          
952600             MOVE SPACE               TO TXT-TEANMNOT-REG(INDX)           
952700           ELSE                                                           
952800           MOVE REQU-TEANMNOT-REG (INDX)                                  
952900                                      TO TXT-TEANMNOT-REG(INDX)           
953000           END-IF                                                         
953100           IF REQU-TEANMNOT-DLR (INDX) = ALL '+'                          
953200             MOVE SPACE               TO TXT-TEANMNOT-DLR(INDX)           
953300           ELSE                                                           
953400             MOVE REQU-TEANMNOT-DLR (INDX)                                
953500                                      TO TXT-TEANMNOT-DLR(INDX)           
953600           END-IF                                                         
953700           IF REQU-TEANMNOT-ADM (INDX) = ALL '+'                          
953800             MOVE SPACE               TO TXT-TEANMNOT-ADM(INDX)           
953900           ELSE                                                           
954000             MOVE REQU-TEANMNOT-ADM (INDX)                                
954100                                      TO TXT-TEANMNOT-ADM(INDX)           
954200           END-IF                                                         
954300           IF REQU-TEANMNOT-REM (INDX) = ALL '+'                          
954400             MOVE SPACE               TO TXT-TEANMNOT-REM(INDX)           
954500           ELSE                                                           
954600             MOVE REQU-TEANMNOT-REM (INDX)                                
954700                                      TO TXT-TEANMNOT-REM(INDX)           
954800           END-IF                                                         
954900           IF REQU-TEANMNOT-RET (INDX) = ALL '+'                          
955000             MOVE SPACE               TO TXT-TEANMNOT-RET(INDX)           
955100           ELSE                                                           
955200             MOVE REQU-TEANMNOT-RET (INDX)                                
955300                                      TO TXT-TEANMNOT-RET(INDX)           
955400           END-IF                                                         
955500                                                                          
955600            ADD +1                    TO INDX                             
955700         END-PERFORM                                                      
955800                                                                          
955900         IF SEGMENT-FINNS                                                 
956000             PERFORM IMS-REPL-WLKREE21                                    
956100         ELSE                                                             
956200             MOVE '1'           TO TXT-KDSEGKEY                           
956300             PERFORM IMS-ISRT-WLKREE21                                    
956400         END-IF                                                           
956500                                                                          
956600         MOVE INF-UPDATE-DONE   TO RESP-IDMSG-INFO                        
956700       ELSE                                                               
956800        MOVE 'IDLEVANM'         TO RESP-IDELMT-ERROR                      
956900        MOVE '025'              TO RESP-IDMSG-ERROR                       
957000         PERFORM FA-TOEM-BILD                                             
957100       END-IF                                                             
957200     END-IF                                                               
957300     .                                                                    
957400     EJECT                                                                
957500                                                                          
957600*    --- DISPATCHER SECTIONS                                              
957700 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
958000                                                                          
960000     MOVE 'GETARG'               TO SUB-KDFUNC                            
970000     MOVE 'CARPARTS.LDC.DISCRLINETEXT'       TO SUB-ADDISPABS             
980000     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
990000                                                                          
000000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
010000                                                                          
020000     IF SUB-KDRC > 0                                                      
030000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
040000       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
050000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
060000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
070000     END-IF                                                               
080000     .                                                                    
090000     SKIP3                                                                
100000 S02-RETURN-RESPONSE SECTION.                                             
110000                                                                          
120000     MOVE 'RETURN'                   TO SUB-KDFUNC                        
130000     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
140000                                                                          
150000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
160000                                                                          
170000     IF SUB-KDRC > 0                                                      
180000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
190000       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
200000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
210000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
220000     END-IF                                                               
230000     .                                                                    
240000     EJECT                                                                
250000* --- IMS SEKTIONER ---                                                   
260000     SKIP3                                                                
640000 IMS-GHU-WLKREE11       SECTION.                                          
650000                                                                          
660000     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
670000          DELIMITED BY SIZE INTO SSA1                                     
680000     STRING 'WLKREE11(WDA211KY =' W-WDA211KY-X ')'                        
690000          DELIMITED BY SIZE INTO SSA2                                     
700000     MOVE '  GE'           TO GODK-STATUSKODER                            
710000     CALL CBLTDLI USING GHU KREE-PCB DLI-IO-AREA SSA1 SSA2                
720000     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
730000     PERFORM IMS-STATUSKONTROLL                                           
740000     .                                                                    
750000                                                                          
760000 IMS-REPL-WLKREE11      SECTION.                                          
770000                                                                          
780000     MOVE '    '           TO GODK-STATUSKODER                            
790000     CALL CBLTDLI USING REPL KREE-PCB DLI-IO-AREA                         
800000     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
810000     PERFORM IMS-STATUSKONTROLL                                           
820000     .                                                                    
830000     EJECT                                                                
840000                                                                          
850000 IMS-GU-WLKREE21       SECTION.                                           
860000                                                                          
870000     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
880000          DELIMITED BY SIZE INTO SSA1                                     
890000     STRING 'WLKREE11(WDA211KY =' W-WDA211KY-X ')'                        
900000          DELIMITED BY SIZE INTO SSA2                                     
910000     STRING 'WLKREE21(KDSEGKEY =' W-KDSEGKEY-X ')'                        
920000          DELIMITED BY SIZE INTO SSA3                                     
930000     MOVE '  GE'           TO GODK-STATUSKODER                            
940000     CALL CBLTDLI USING GU KREE-PCB DLI-IO-AREA SSA1 SSA2 SSA3            
950000     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
960000     PERFORM IMS-STATUSKONTROLL                                           
970000     .                                                                    
980000                                                                          
990000 IMS-GHNP-WLKREE21       SECTION.                                         
000000                                                                          
010000     STRING 'WLKREE21(KDSEGKEY =' W-KDSEGKEY-X ')'                        
020000          DELIMITED BY SIZE INTO SSA1                                     
030000     MOVE '  GE'           TO GODK-STATUSKODER                            
040000     CALL CBLTDLI USING GHNP KREE-PCB DLI-IO-AREA SSA1                    
050000     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
060000     PERFORM IMS-STATUSKONTROLL                                           
070000     .                                                                    
080000                                                                          
090000 IMS-ISRT-WLKREE21       SECTION.                                         
100000                                                                          
110000     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
120000          DELIMITED BY SIZE INTO SSA1                                     
130000     STRING 'WLKREE11(WDA211KY =' W-WDA211KY-X ')'                        
140000          DELIMITED BY SIZE INTO SSA2                                     
150000     MOVE 'WLKREE21'           TO SSA3                                    
160000     MOVE '    '           TO GODK-STATUSKODER                            
170000     CALL CBLTDLI USING ISRT KREE-PCB DLI-IO-AREA SSA1 SSA2 SSA3          
180000     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
190000     PERFORM IMS-STATUSKONTROLL                                           
200000     .                                                                    
210000                                                                          
220000 IMS-REPL-WLKREE21      SECTION.                                          
230000                                                                          
240000     MOVE '    '           TO GODK-STATUSKODER                            
250000     CALL CBLTDLI USING REPL KREE-PCB DLI-IO-AREA                         
260000     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
270000     PERFORM IMS-STATUSKONTROLL                                           
280000     .                                                                    
290000     EJECT                                                                
300000                                                                          
301000 IMS-GU-WDA201          SECTION.                                          
302000                                                                          
303000     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
304000        DELIMITED BY SIZE INTO SSA1                                       
305000     MOVE '  GE' TO GODK-STATUSKODER                                      
306000     CALL CBLTDLI USING GU KREE-PCB DLI-IO-WDA201 SSA1                    
307000     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
308000     PERFORM IMS-STATUSKONTROLL                                           
309000     .                                                                    
309100     EJECT                                                                
309200                                                                          
320000 IMS-STATUSKONTROLL SECTION.                                              
330000                                                                          
340000     SET STATUS-IX TO 1                                                   
350000     SEARCH GODK-STATUS                                                   
360000       AT END                                                             
370000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
380000         DELIMITED BY SIZE INTO FELTEXT                                   
390000         CALL FELLOG                                                      
400000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
410000         CONTINUE                                                         
420000     END-SEARCH                                                           
430000     .                                                                    
