000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL014300.                                                
000300 AUTHOR.         SUBBARAO PARUCHURI V.                                    
000400 DATE-WRITTEN.   04/07/14.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       'CARPARTS.LDC.ORDERQUERYCASE'                            
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        'CARPARTS.LDC.ORDERQUERYCASE'                                    
001100*                                                                         
001200*                                                                         
001300*    WL014300 PROGRAM IS A REPLICA OF W4050800 PROGRAM                    
001400*    AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                              
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSACTION: WL0143T                                             
001800*        REQUEST:     WL0143I1                                            
001900*                                                                         
002000*    OUTDATA.                                                             
002100*        RESPONSE:    WL0143O1                                            
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500 77  IDPGM                       PIC X(08)   VALUE 'WL014300'.            
003600                                                                          
003700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003800 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003900 77  KDRC-DISPLAY                PIC Z(5).                                
004000                                                                          
004100 77  JA                        PIC X       VALUE 'J'.                     
004200 77  NEJ                       PIC X       VALUE 'N'.                     
004300 77  NYCKLAR-OK                PIC X       VALUE 'J'.                     
004400 77  PRODNR-OK                 PIC X       VALUE 'J'.                     
004500 77  ORDERNR-OK                PIC X       VALUE 'J'.                     
004600 77  IDORDNR7-WS               PIC X(7)    VALUE SPACE.                   
004700 77  IDPRODNR-WS               PIC X(7)    VALUE SPACE.                   
004800 77  IDKOLLI-WS                PIC X(5)    VALUE SPACE.                   
004900 77  IDARTNR-WS                PIC X(9)    VALUE SPACE.                   
005000 77  IX                        PIC S9(9)   VALUE +0    COMP SYNC.         
005100 77  MAX-RADER                 PIC S9(3)   VALUE +500  COMP SYNC.         
005200 77  OK-VISA-ALLT              PIC X       VALUE ' '.                     
005300 77  OBEHORIG                  PIC X       VALUE 'F'.                     
005400 77  IMPORTER                  PIC X       VALUE '1'.                     
005500 77  DEALER                    PIC X       VALUE '2'.                     
005600 77  INTERNDISTRIKT            PIC X       VALUE '3'.                     
005700 77  WS-COUNT                  PIC 9(3)    VALUE ZERO.                    
005800 77  WS-IDELMT-ERROR             PIC X(16).                               
005900 77  WS-IDMSG-ERROR              PIC X(03).                               
006000 77  WS-IDMSG-INFO               PIC X(03).                               
006100*      --- VALID IDDC CODES                                               
006200*                                                                         
006300 77  WS-VKORDBTO                 PIC S9(6)V9.                             
006400 77  WS-VLORDBTO                 PIC S9(4)V9(3).                          
006500                                                                          
006600 01  WS-KDMATT                   PIC X.                                   
006700     88 US-MEASUREMENT           VALUE 'U'.                               
006800     88 SIS-MEASUREMENT          VALUE 'S'.                               
006900                                                                          
007000 77  AVERAGECOST-SW             PIC X      VALUE 'J'.                     
007100     88  AVERAGECOST                       VALUE 'J'.                     
007200                                                                          
007300 77  BOUNCE-SW                  PIC X      VALUE 'J'.                     
007400     88  BOUNCEORDER                       VALUE 'J'.                     
007500                                                                          
007600 77  VISA-ORDER-SW              PIC X      VALUE 'J'.                     
007700     88  VISA-ORDER                        VALUE 'J'.                     
007800                                                                          
007900 01  W-SPAR-IDKUNDRF.                                                     
008000     03  W-SPAR-IDORDNR7         PIC X(7)    VALUE '+++++++'.             
008100     03  FILLER                  PIC X(3)    VALUE '+++'.                 
008200                                                                          
008300     EJECT                                                                
008400 01  IDDISTR.                                                             
008500     03  IDDISTR-WS                PIC X(4)    VALUE SPACE.               
008600     03  IDDISTR-NUM REDEFINES IDDISTR-WS PIC 9(4).                       
008700                                                                          
008800 01  IDKUNDNR.                                                            
008900     03  IDKUNDNR-WS               PIC X(6)    VALUE SPACE.               
009000     03  IDKUNDNR-NUM REDEFINES IDKUNDNR-WS PIC 9(6).                     
009100 01  ADFLOMR.                                                             
009200     03  ADFLOMR1-5                PIC X(5)    VALUE SPACE.               
009300     03 FILLER REDEFINES ADFLOMR1-5.                                      
009400       05  ADFLOMR1                PIC X.                                 
009500       05  ADFLOMR2-4              PIC X(3).                              
009600       05  ADFLOMR5                PIC X.                                 
009700     EJECT                                                                
009800 01  FILLER                      PIC X(16)     VALUE 'SPAR AREA'.         
009900                                                                          
010000 01  SPAR-IDPRODNR               PIC S9(7)      VALUE ZERO COMP-3.        
010100 01  SPAR-KVKOLLI                PIC S9(5)      VALUE ZERO COMP-3.        
010200 01  SPAR-KVKOLLI-FAKT           PIC S9(5)      VALUE ZERO COMP-3.        
010300 01  SPAR-KVKOLLI-LAST           PIC S9(5)      VALUE ZERO COMP-3.        
010400 01  SPAR-KVORDRAD               PIC S9(5)      VALUE ZERO COMP-3.        
010500 01  SPAR-SUORDV                 PIC S9(9)V9(2) VALUE ZERO COMP-3.        
010600 01  SPAR-SUORDV-LOC             PIC S9(9)V9(2) VALUE ZERO COMP-3.        
010700 01  SPAR-SUORDV-LOCPREL         PIC S9(9)V9(2) VALUE ZERO COMP-3.        
010800 01  SPAR-VKORDBTO               PIC S9(6)V9(1) VALUE ZERO COMP-3.        
010900 01  SPAR-VLORDBTO               PIC S9(4)V9(3) VALUE ZERO COMP-3.        
011000 01  HELP-SUMMA                  PIC S9(9)V9(2) VALUE ZERO COMP-3.        
011100     EJECT                                                                
011200 01    NYCKLAR-TILL-DLI.                                                  
011300     03  W-WDE4A1KY-MIN-X.                                                
011400         05 W-IDDISTR-MIN        PIC S9(5)   VALUE ZERO  COMP-3.          
011500         05 W-IDKUNDNR-MIN       PIC S9(7)   VALUE ZERO  COMP-3.          
011600         05 W-IDKUNDRF-MIN.                                               
011700            07 W-IDORDNR5-MIN    PIC  9(5)   VALUE ZERO.                  
011800            07 FILLER            PIC  X(5)   VALUE SPACE.                 
011900         05 W-IDPRODNR-MIN       PIC S9(7)   VALUE ZERO  COMP-3.          
012000         05 FILLER               PIC  X(2)   VALUE LOW-VALUE.             
012100                                                                          
012200     03  W-WDE4A1KY-MAX-X.                                                
012300         05 W-IDDISTR-MAX        PIC S9(5)   VALUE ZERO  COMP-3.          
012400         05 W-IDKUNDNR-MAX       PIC S9(7)   VALUE ZERO  COMP-3.          
012500         05 W-IDKUNDRF-MAX.                                               
012600            07 W-IDORDNR5-MAX    PIC  9(5)   VALUE ZERO.                  
012700            07 FILLER            PIC  X(5)   VALUE SPACE.                 
012800         05 W-IDPRODNR-MAX       PIC S9(7)  VALUE +9999999 COMP-3.        
012900         05 FILLER               PIC  X(2)   VALUE HIGH-VALUE.            
013000                                                                          
013100     03  W-IDPRODNR-X.                                                    
013200         05 W-IDPRODNR           PIC S9(7)   VALUE ZERO  COMP-3.          
013300                                                                          
013400     03  W-IDKOLLI-MIN-X.                                                 
013500         05 W-IDKOLLI-MIN        PIC S9(5)   VALUE ZERO  COMP-3.          
013600     03  W-IDKOLLI-MAX-X.                                                 
013700         05 W-IDKOLLI-MAX        PIC S9(5)   VALUE ZERO  COMP-3.          
013800                                                                          
013900*                                                                         
014000     EJECT                                                                
015000 01  TEST-IDDISTR                PIC  9(5)   COMP-3.                      
016000     SKIP3                                                                
016100 01  FILLER REDEFINES TEST-IDDISTR.                                       
016200*  03 -COPY WWDIST03.                                                     
016300 01  FILLER REDEFINES TEST-IDDISTR.                                       
016400*  03 -COPY WWDIST79                                                      
016500*     ----DISTR-DEALER-PRICE----                                          
016600 01   -COPY WWDCKONS                                                      
016700*     ----VALID DC'S        ----                                          
016800     EJECT                                                                
016900 01    MEDDELANDE.                                                        
017000   03   FEL-X1                   PIC X(03)       VALUE '043'.             
017100   03   FEL-X2                   PIC X(03)       VALUE '125'.             
017200   03   FEL-X3                   PIC X(03)       VALUE '169'.             
017300   03   FEL-X4                   PIC X(03)       VALUE '00A'.             
017400   03   FEL-X5                   PIC X(03)       VALUE '204'.             
017500   03   FEL-X6                   PIC X(03)       VALUE '099'.             
017600   03   FEL-X7                   PIC X(03)       VALUE '028'.             
017700     EJECT                                                                
017800*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
017900 01  GENERAL-SUBPROGRAMS.                                                 
018000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
018100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
018200     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
018300     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
018400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
018500     03  WWOMVAND                PIC X(8)    VALUE 'WWOMVAND'.            
018600                                                                          
018700*    --- PARAMETERS TO ABEND                                              
018800                                                                          
018900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
019000 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
019100 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
019200     EJECT                                                                
019300*                                                                         
019400 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
019500     SKIP3                                                                
019600*01  -COPY WZ01SUB                                                        
019700     EJECT                                                                
019800 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
019900     SKIP3                                                                
020000 01  REQU-AREA.                                                           
020100*    03  -COPY WZ01REQU                                                   
020200*    03  -COPY WL0143I1                                                   
020300     EJECT                                                                
020400 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
020500     SKIP3                                                                
020600 01  RESP-AREA.                                                           
020700*    03  -COPY WZ01RESP                                                   
020800*    03  -COPY WL0143O1                                                   
020900     EJECT                                                                
021000     SKIP2                                                                
021100 01    FILLER                  PIC X(16)   VALUE 'WWOMVAND '.             
021200*01   -COPY WWOMVAND                                                      
021300     SKIP2                                                                
021400*01   -COPY WSECAREA                                                      
021500     EJECT                                                                
021600******************************************************************        
021700*                                                                         
021800*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021900*                                                                         
022000 01    IMS-WS.                                                            
022100   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
022200     SKIP3                                                                
022300*                        **** STATUS-KOD FRÅN IMS                         
022400   03    STATUS-WS               PIC XX.                                  
022500     88    SEGMENT-FINNS                     VALUE '  '.                  
022600     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
022700     88    BASEN-SLUT                        VALUE 'GB'.                  
022800     SKIP3                                                                
022900   03    GODK-STATUSKODER.                                                
023000     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
023100     SKIP3                                                                
023200 01    SSA1                      PIC X(96).                               
023300 01    SSA2                      PIC X(96).                               
023400     EJECT                                                                
023500*                            IMS FUNKTIONSKODER                           
023600*01    -COPY W0003                                                        
023700     EJECT                                                                
023800*                            DLI INPUT-OUTPUT AREA                        
023900 01  FILLER                      PIC X(16) VALUE 'DLI-WDE4A1'.            
024000                                                                          
024100 01    DLI-IO-AREA-WDE4A1.                                                
024200*  03             -COPY WDE4A1                                            
024300     EJECT                                                                
024400 01  FILLER                      PIC X(16) VALUE 'DLI-WDE601'.            
024500                                                                          
024600 01    DLI-IO-AREA-WDE601.                                                
024700*  03             -COPY WDE601                                            
024800     EJECT                                                                
024900 01  FILLER                      PIC X(16) VALUE 'DLI-WDE611'.            
025000                                                                          
025100 01    DLI-IO-AREA-WDE611.                                                
025200*  03             -COPY WDE611                                            
025300     EJECT                                                                
025400 LINKAGE SECTION.                                                         
025500 01  MSG-PCB                     PIC X.                                   
025600     EJECT                                                                
025700*01    -COPY W0008     -PRE WDE6-                                         
025800     05  FILLER                  PIC X.                                   
025900     SKIP3                                                                
026000*01    -COPY W0008     -PRE WDE4A-                                        
027000     05  FILLER                  PIC X.                                   
028000     EJECT                                                                
028100 PROCEDURE DIVISION USING  MSG-PCB  WDE6-PCB  WDE4A-PCB.                  
028200 MAIN SECTION.                                                            
028300     ENTRY 'DLITCBL' USING MSG-PCB  WDE6-PCB  WDE4A-PCB.                  
028400                                                                          
028500     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
028600     IF SUB-KDRC = 0                                                      
028700      IF  REQU-KDPGMACT  = 'S'                                            
028800       PERFORM A-INIT-SPARA-INPUT                                         
028900       PERFORM B-KOLLA-NYCKLAR                                            
029000       IF REQU-IDARTNR-KEY   NUMERIC OR                                   
030000          REQU-IDARTNR-KEY = ALL '+'                                      
030100          CONTINUE                                                        
030200       ELSE                                                               
030300         MOVE NEJ        TO NYCKLAR-OK                                    
030400                            PRODNR-OK                                     
030500                            ORDERNR-OK                                    
030600       END-IF                                                             
030700                                                                          
030800       IF REQU-IDKOLLI-KEY   NUMERIC OR                                   
030900          REQU-IDKOLLI-KEY = ALL '+'                                      
031000          CONTINUE                                                        
031100       ELSE                                                               
031200         MOVE NEJ        TO NYCKLAR-OK                                    
031300                            PRODNR-OK                                     
031400                            ORDERNR-OK                                    
031500       END-IF                                                             
031600                                                                          
031700       IF REQU-IDPRODNR-KEY  NUMERIC OR                                   
031800          REQU-IDPRODNR-KEY = ALL '+'                                     
031900          CONTINUE                                                        
032000       ELSE                                                               
032100         MOVE NEJ        TO NYCKLAR-OK                                    
032200                            PRODNR-OK                                     
032300                            ORDERNR-OK                                    
032400       END-IF                                                             
032500                                                                          
032600       IF NYCKLAR-OK = JA                                                 
032700         PERFORM C-KONTROLL-SECURIT                                       
032800       END-IF                                                             
032900                                                                          
033000       IF SEC-KDSVAR = OBEHORIG                                           
033100         MOVE FEL-X4                       TO RESP-IDMSG-ERROR            
033200       ELSE                                                               
033300                                                                          
033400         EVALUATE TRUE                                                    
033500           WHEN PRODNR-OK = JA                                            
033600             PERFORM D-LAES-WDE6                                          
033700           WHEN ORDERNR-OK = JA                                           
033800             PERFORM E-LAES-WDE4A                                         
033900           WHEN OTHER                                                     
034000             MOVE FEL-X1              TO RESP-IDMSG-ERROR                 
034100             MOVE +1 TO IX                                                
034200             PERFORM UNTIL IX > MAX-RADER                                 
034300               PERFORM S02-BLANKA-RADER                                   
034400               ADD +1 TO IX                                               
034500             END-PERFORM                                                  
034600         END-EVALUATE                                                     
034700       END-IF                                                             
034800                                                                          
034900       PERFORM F-SPARA-NYCKLAR                                            
035000                                                                          
035100      ELSE                                                                
035200        MOVE FEL-X6                     TO RESP-IDMSG-ERROR               
035300      END-IF                                                              
035400       MOVE RESP-IDMSG-INFO    TO WS-IDMSG-INFO                           
035500       MOVE RESP-IDMSG-ERROR   TO WS-IDMSG-ERROR                          
035600       MOVE RESP-IDELMT-ERROR  TO WS-IDELMT-ERROR                         
035700       IF WS-IDMSG-ERROR NOT = SPACE                                      
035800           MOVE ALL '+' TO RESP-AREA                                      
035900           MOVE WS-IDMSG-ERROR   TO RESP-IDMSG-ERROR                      
036000           MOVE WS-IDELMT-ERROR  TO RESP-IDELMT-ERROR                     
036100           MOVE WS-IDMSG-INFO    TO RESP-IDMSG-INFO                       
036200           MOVE 001              TO RESP-IDMSGVER                         
036300           MOVE ZERO             TO RESP-KVRADER                          
036400       END-IF                                                             
036500       PERFORM S02-RETURN-RESPONSE                                        
036600     END-IF                                                               
036700     MOVE ZERO TO RETURN-CODE                                             
036800     GOBACK                                                               
036900     .                                                                    
037000     EJECT                                                                
037100 A-INIT-SPARA-INPUT SECTION.                                              
037200                                                                          
037300     MOVE SPACE                            TO SEC-KDSVAR                  
037400     MOVE ALL '+'            TO RESP-AREA                                 
037500     MOVE SPACE              TO RESP-IDMSG-ERROR                          
037600                                RESP-IDMSG-INFO                           
037700                                RESP-IDELMT-ERROR                         
037800     MOVE 001                TO RESP-IDMSGVER                             
037900     MOVE ZERO               TO RESP-KVRADER                              
038000     .                                                                    
038100     EJECT                                                                
038200 B-KOLLA-NYCKLAR SECTION.                                                 
038300                                                                          
038400                                                                          
038500     MOVE REQU-KDMATT               TO WS-KDMATT                          
038600     MOVE REQU-IDPRODNR-KEY         TO IDPRODNR-WS                        
038700                                                                          
038800     MOVE REQU-IDKOLLI-KEY          TO IDKOLLI-WS                         
038900                                                                          
039000     MOVE ZERO                      TO W-IDKOLLI-MIN                      
039100     MOVE +99999                    TO W-IDKOLLI-MAX                      
039200     MOVE NEJ                       TO NYCKLAR-OK                         
039300                                       PRODNR-OK                          
039400                                       ORDERNR-OK                         
039500                                                                          
039600     IF IDPRODNR-WS NUMERIC AND                                           
039700        IDPRODNR-WS NOT = ZERO                                            
039800       MOVE IDPRODNR-WS                  TO W-IDPRODNR                    
039900       MOVE ZERO                         TO IDORDNR7-WS                   
040000       MOVE JA                           TO PRODNR-OK                     
040100                                            NYCKLAR-OK                    
040200       MOVE NEJ                          TO ORDERNR-OK                    
040300     ELSE                                                                 
040400                                                                          
040500       IF REQU-IDDISTR-KEY  NUMERIC    AND                                
040600          REQU-IDDISTR-KEY    NOT = ZERO AND                              
040700          REQU-IDKUNDNR-KEY NUMERIC   AND                                 
040800          REQU-IDORDNR7-KEY(1:7) NUMERIC                                  
040900         MOVE REQU-IDDISTR-KEY             TO W-IDDISTR-MIN               
041000                                              W-IDDISTR-MAX               
041100                                              TEST-IDDISTR                
041200         MOVE REQU-IDKUNDNR-KEY            TO W-IDKUNDNR-MIN              
041300                                              W-IDKUNDNR-MAX              
041400         MOVE REQU-IDORDNR7-KEY(3:5)       TO W-IDORDNR5-MIN              
041500                                              W-IDORDNR5-MAX              
041600         MOVE JA                           TO ORDERNR-OK                  
041700                                              NYCKLAR-OK                  
041800       END-IF                                                             
041900     END-IF                                                               
042000                                                                          
042100     IF DIST79-DEALER-PRICE                                               
042200       MOVE 'DEALERPRICE'    TO RESP-TEDDI                                
042300     ELSE                                                                 
042400       MOVE SPACE              TO RESP-TEDDI                              
042500     END-IF                                                               
042600                                                                          
042700     IF IDKOLLI-WS NOT NUMERIC                                            
042800       MOVE ZERO               TO IDKOLLI-WS                              
042900     ELSE                                                                 
043000       IF IDKOLLI-WS NUMERIC AND                                          
044000          IDKOLLI-WS NOT = ZERO                                           
044100         MOVE IDKOLLI-WS       TO W-IDKOLLI-MIN                           
044200                                  W-IDKOLLI-MAX                           
044300       END-IF                                                             
044400     END-IF                                                               
044500                                                                          
044600     MOVE REQU-IDDC-KEY        TO RESP-IDDC-KEY                           
044700     MOVE REQU-IDARTNR-KEY     TO IDARTNR-WS                              
044800     .                                                                    
044900     EJECT                                                                
045000 C-KONTROLL-SECURIT SECTION.                                              
045100                                                                          
045200     MOVE REQU-IDUSER                  TO SEC-IDUSER                      
045300*THIS PROGRAM IS A COPY OF W4050800 PROGRAM  AND MODIFIED FOR             
045400*LDC PROJECT                                                              
045500     MOVE '4508'                       TO SEC-IDTRANS                     
045600     MOVE REQU-IDDISTR-KEY             TO SEC-IDKEY                       
045700                                                                          
045800     CALL WSECURIT USING                  SEC-IDUSER                      
045900                                          SEC-IDTRANS                     
046000                                          SEC-IDKEY                       
046100                                          SEC-KDSVAR                      
046200     .                                                                    
046300     EJECT                                                                
046400 D-LAES-WDE6 SECTION.                                                     
046500                                                                          
046600     MOVE JA  TO VISA-ORDER-SW                                            
046700     MOVE NEJ TO BOUNCE-SW                                                
046800     MOVE +1 TO IX                                                        
046900     PERFORM IMS-GU-E601-PRODNR-UNIK                                      
047000                                                                          
047100     IF SEGMENT-FINNS                                                     
047200       IF VORD-IDDC-EXP NOT = SPACE                                       
047300          MOVE JA       TO BOUNCE-SW                                      
047400          IF VORD-IDDC-EXP = WC-CDC-SE                                    
047500*           *BOUNCE DC 11                                                 
047600            IF VORD-IDDC = REQU-IDDC-KEY                                  
047700               MOVE JA  TO AVERAGECOST-SW                                 
047800            ELSE                                                          
047900               MOVE NEJ TO AVERAGECOST-SW                                 
048000            END-IF                                                        
048100          ELSE                                                            
048200*           *BOUNCE DC NOT 11                                             
048300            IF VORD-IDDC = REQU-IDDC-KEY                                  
048400               MOVE NEJ TO AVERAGECOST-SW                                 
048500            ELSE                                                          
048600               MOVE JA  TO AVERAGECOST-SW                                 
048700               IF VORD-IDDC-EXP = REQU-IDDC-KEY                           
048800                  AND VORD-KVKOLLI-FAKT = 0                               
048900                 MOVE NEJ TO VISA-ORDER-SW                                
049000               END-IF                                                     
049100            END-IF                                                        
049200          END-IF                                                          
049300       ELSE                                                               
049400          IF VORD-SUORDV-EXP > 0                                          
049500             MOVE JA  TO AVERAGECOST-SW                                   
049600          ELSE                                                            
049700             MOVE NEJ TO AVERAGECOST-SW                                   
049800          END-IF                                                          
049900       END-IF                                                             
050000       IF VISA-ORDER                                                      
050100         PERFORM DB-FLYTTA-HUVUD                                          
050200         PERFORM DC-HAEMTA-ORDERNR                                        
050300         PERFORM IMS-GNP-E611-KOLLI                                       
050400         PERFORM DD-SPARA-ENTER-NYCKLAR                                   
050500                                                                          
050600         PERFORM UNTIL IX > MAX-RADER                                     
050700           IF SEGMENT-FINNS                                               
050800             PERFORM DE-BEHANDLA-KOLLI-RADER                              
050900                                                                          
051000           ELSE                                                           
051100             IF IX = +1                                                   
051200               MOVE FEL-X3              TO RESP-IDMSG-ERROR               
051300             END-IF                                                       
051400             PERFORM S02-BLANKA-RADER                                     
051500           END-IF                                                         
051600           ADD +1 TO IX                                                   
051700         END-PERFORM                                                      
051800       ELSE                                                               
051900         MOVE 'IDORDNR'           TO RESP-IDELMT-ERROR                    
052000         MOVE '025'               TO RESP-IDMSG-ERROR                     
052100         PERFORM UNTIL IX > MAX-RADER                                     
052200           PERFORM S02-BLANKA-RADER                                       
052300           ADD +1 TO IX                                                   
052400         END-PERFORM                                                      
052500       END-IF                                                             
052600                                                                          
052700     ELSE                                                                 
052800       MOVE 'IDORDNR'           TO RESP-IDELMT-ERROR                      
052900       MOVE '025'               TO RESP-IDMSG-ERROR                       
053000       PERFORM UNTIL IX > MAX-RADER                                       
053100         PERFORM S02-BLANKA-RADER                                         
053200         ADD +1 TO IX                                                     
053300       END-PERFORM                                                        
053400     END-IF                                                               
053500     .                                                                    
053600     EJECT                                                                
053700 DB-FLYTTA-HUVUD SECTION.                                                 
053800                                                                          
053900     MOVE VORD-IDDISTR                 TO IDDISTR-NUM                     
054000     MOVE VORD-IDKUNDNR                TO IDKUNDNR-NUM                    
054100     MOVE VORD-KVKOLLI                 TO RESP-KVKOLLI                    
054200     MOVE VORD-KVKOLLI-FAKT            TO RESP-KVKOLLI-FAKT               
054300     MOVE VORD-KVKOLLI-LAST            TO RESP-KVKOLLI-LAST               
054400     MOVE VORD-KVORDRAD                TO RESP-KVORDRAD-TOT               
054500                                                                          
054600     IF SEC-KDSVAR = 2 OR 6                                               
054700        MOVE ZERO                      TO RESP-SUORDV                     
054800     ELSE                                                                 
054900        IF DIST79-DEALER-PRICE                                            
055000          COMPUTE HELP-SUMMA = VORD-SUORDV-LOC +                          
055100                               VORD-SUORDV-LOCPREL                        
055200          MOVE HELP-SUMMA              TO RESP-SUORDV                     
055300          MOVE VORD-KDVALISO           TO RESP-TEDDI                      
055400          IF VORD-SUORDV-LOCPREL = +0                                     
055500            MOVE ' '                   TO RESP-TEASTRIX                   
055600          ELSE                                                            
055700            MOVE '*'                   TO RESP-TEASTRIX                   
055800          END-IF                                                          
055900        ELSE                                                              
056000          IF AVERAGECOST                                                  
056100             MOVE VORD-SUORDV-EXP      TO RESP-SUORDV                     
056200             MOVE VORD-KDVALISO-EXP    TO RESP-TEDDI                      
056300          ELSE                                                            
056400             MOVE VORD-SUORDV          TO RESP-SUORDV                     
056500             MOVE VORD-KDVALISO        TO RESP-TEDDI                      
056600          END-IF                                                          
056700          MOVE ' '                     TO RESP-TEASTRIX                   
056800        END-IF                                                            
056900     END-IF                                                               
057000                                                                          
057100     MOVE VORD-VLORDBTO      TO WS-VLORDBTO                               
057200     MOVE VORD-VKORDBTO      TO WS-VKORDBTO                               
057300     IF US-MEASUREMENT                                                    
057400       COMPUTE WS-VLORDBTO ROUNDED =                                      
057500               WS-VLORDBTO * CONV-M3-TO-FT3 END-COMPUTE                   
057600       COMPUTE WS-VKORDBTO ROUNDED =                                      
057700               WS-VKORDBTO * CONV-KG-TO-LB  END-COMPUTE                   
057800     END-IF                                                               
057900     MOVE WS-VLORDBTO        TO RESP-VLORDBTO                             
058000     MOVE WS-VKORDBTO        TO RESP-VKORDBTO                             
058100                                                                          
058200     MOVE VORD-IDDISTR                 TO TEST-IDDISTR                    
058300     .                                                                    
058400     EJECT                                                                
058500 DC-HAEMTA-ORDERNR       SECTION.                                         
058600                                                                          
058700     MOVE LOW-VALUE       TO W-WDE4A1KY-MIN-X                             
058800     MOVE HIGH-VALUE      TO W-WDE4A1KY-MAX-X                             
058900                                                                          
059000     MOVE VORD-IDDISTR    TO W-IDDISTR-MIN                                
059100                             W-IDDISTR-MAX                                
059200                                                                          
059300     MOVE VORD-IDKUNDNR   TO W-IDKUNDNR-MIN                               
059400                             W-IDKUNDNR-MAX                               
059500                                                                          
059600     PERFORM IMS-GU-E4A1-ORDNR                                            
059700     IF SEGMENT-FINNS                                                     
059800         PERFORM UNTIL VORD-IDPRODNR = SEQA-IDPRODNR                      
059900           PERFORM IMS-GN-E4A1-ORDNR                                      
060000         END-PERFORM                                                      
060100         MOVE SEQA-IDDISTR  TO IDDISTR-NUM                                
060200                               RESP-IDDISTR-KEY                           
060300                                                                          
060400         MOVE SEQA-IDKUNDNR TO IDKUNDNR-NUM                               
060500     ELSE                                                                 
060600        CALL FELLOG                                                       
060700     END-IF                                                               
060800     .                                                                    
060900     EJECT                                                                
061000 DD-SPARA-ENTER-NYCKLAR SECTION.                                          
061100                                                                          
061200     IF SEGMENT-FINNS                                                     
061300       CONTINUE                                                           
061400     END-IF                                                               
061500     .                                                                    
061600     EJECT                                                                
061700 DE-BEHANDLA-KOLLI-RADER SECTION.                                         
061800                                                                          
061900     IF DIST03-SVERIGE-2 AND                                              
062000        KOLLI-IDKOLLI > +99000                                            
062100       CONTINUE                                                           
062200                                                                          
062300     ELSE                                                                 
062400       IF IX NOT > MAX-RADER                                              
062500         IF KOLLI-IDKOLLI NOT < ZERO                                      
062600           PERFORM S01-FLYTTA-KOLLI-RADER                                 
062700                                                                          
062800         ELSE                                                             
062900           SUBTRACT +1 FROM IX                                            
063000         END-IF                                                           
063100         PERFORM IMS-GNP-E611-KOLLI                                       
063200                                                                          
063300       ELSE                                                               
063400         MOVE FEL-X5              TO RESP-IDMSG-INFO                      
063500       END-IF                                                             
063600     END-IF                                                               
063700     .                                                                    
063800     EJECT                                                                
063900 E-LAES-WDE4A      SECTION.                                               
064000                                                                          
064100     MOVE JA              TO VISA-ORDER-SW                                
064200     MOVE NEJ             TO BOUNCE-SW                                    
064300     MOVE +1 TO IX                                                        
064400     PERFORM IMS-GU-E4A1-ORDNR                                            
064500                                                                          
064600     IF SEGMENT-FINNS                                                     
064700       PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                         
064800         IF SPAR-IDPRODNR NOT = SEQA-IDPRODNR                             
064900           MOVE SEQA-IDPRODNR TO W-IDPRODNR                               
065000           PERFORM IMS-GU-E601-PRODNR                                     
065100           IF VORD-IDDC-EXP NOT = SPACE                                   
065200              MOVE JA     TO BOUNCE-SW                                    
065300              IF VORD-IDDC-EXP = WC-CDC-SE                                
065400*               *BOUNCE DC 11                                             
065500                IF VORD-IDDC = REQU-IDDC-KEY                              
065600                   MOVE JA  TO AVERAGECOST-SW                             
065700                ELSE                                                      
065800                   MOVE NEJ TO AVERAGECOST-SW                             
065900                END-IF                                                    
066000              ELSE                                                        
066100*               *BOUNCE DC NOT 11                                         
066200                IF VORD-IDDC = REQU-IDDC-KEY                              
066300                   MOVE NEJ TO AVERAGECOST-SW                             
066400                ELSE                                                      
066500                   MOVE JA  TO AVERAGECOST-SW                             
066600                   IF VORD-IDDC-EXP = REQU-IDDC-KEY                       
066700                      AND VORD-KVKOLLI-FAKT = 0                           
066800                      MOVE NEJ TO VISA-ORDER-SW                           
066900                   END-IF                                                 
067000                END-IF                                                    
067100              END-IF                                                      
067200           ELSE                                                           
067300              IF VORD-SUORDV-EXP > 0                                      
067400                 MOVE JA  TO AVERAGECOST-SW                               
067500              ELSE                                                        
067600                 MOVE NEJ TO AVERAGECOST-SW                               
067700              END-IF                                                      
067800           END-IF                                                         
067900           IF VISA-ORDER                                                  
068000             PERFORM EB-BERAEKNA-HUVUD-TOTALER                            
068100                                                                          
068200             IF IX > MAX-RADER                                            
068300               PERFORM EC-SPARA-NYCKLAR-FOER-PF8                          
068400                                                                          
068500             ELSE                                                         
068600               PERFORM ED-LAES-BEHANDLA-KOLLI                             
068700             END-IF                                                       
068800           END-IF                                                         
068900         END-IF                                                           
069000         MOVE SEQA-IDPRODNR TO SPAR-IDPRODNR                              
069100         PERFORM IMS-GN-E4A1-ORDNR                                        
069200       END-PERFORM                                                        
069300                                                                          
069400       IF IX = +1                                                         
069500         MOVE FEL-X3              TO RESP-IDMSG-ERROR                     
069600       END-IF                                                             
069700       PERFORM EE-VISA-BILD                                               
069800                                                                          
069900     ELSE                                                                 
070000       PERFORM EF-VISA-ORDER-SAKNAS                                       
070100     END-IF                                                               
070200     .                                                                    
070300     EJECT                                                                
070400 EB-BERAEKNA-HUVUD-TOTALER SECTION.                                       
070500                                                                          
070600     MOVE VORD-IDDISTR      TO IDDISTR-NUM                                
070700                               TEST-IDDISTR                               
070800     MOVE VORD-IDKUNDNR     TO IDKUNDNR-NUM                               
070900                                                                          
071000     ADD VORD-KVKOLLI       TO SPAR-KVKOLLI                               
071100     ADD VORD-KVKOLLI-FAKT  TO SPAR-KVKOLLI-FAKT                          
071200     ADD VORD-KVKOLLI-LAST  TO SPAR-KVKOLLI-LAST                          
071300     ADD VORD-KVORDRAD      TO SPAR-KVORDRAD                              
071400     IF AVERAGECOST                                                       
071500        ADD VORD-SUORDV-EXP TO SPAR-SUORDV                                
071600        MOVE VORD-KDVALISO-EXP TO RESP-TEDDI                              
071700     ELSE                                                                 
071800        ADD VORD-SUORDV     TO SPAR-SUORDV                                
071900        MOVE VORD-KDVALISO  TO RESP-TEDDI                                 
072000     END-IF                                                               
072100     ADD VORD-SUORDV-LOC    TO SPAR-SUORDV-LOC                            
072200     ADD VORD-SUORDV-LOCPREL TO SPAR-SUORDV-LOCPREL                       
072300     ADD VORD-VKORDBTO      TO SPAR-VKORDBTO                              
072400     ADD VORD-VLORDBTO      TO SPAR-VLORDBTO                              
072500                                                                          
072600                                                                          
072700     MOVE VORD-IDDISTR      TO TEST-IDDISTR                               
072800     .                                                                    
072900     EJECT                                                                
073000 EC-SPARA-NYCKLAR-FOER-PF8 SECTION.                                       
073100                                                                          
073200     IF IX > MAX-RADER                                                    
073300                                                                          
073400         MOVE FEL-X5              TO RESP-IDMSG-INFO                      
073500     END-IF                                                               
073600     .                                                                    
073700     EJECT                                                                
073800 ED-LAES-BEHANDLA-KOLLI SECTION.                                          
073900                                                                          
074000     IF (IDKOLLI-WS > ZERO AND VORD-FLDIRLEV = NEJ) OR                    
074100         IDKOLLI-WS = ZERO                                                
074200       PERFORM IMS-GNP-E611-KOLLI                                         
074300                                                                          
074400       IF SEGMENT-FINNS                                                   
074500                                                                          
074600         PERFORM UNTIL SEGMENT-SAKNAS OR IX > MAX-RADER                   
074700            PERFORM EDB-BEHANDLA-KOLLI-RADER                              
074800         END-PERFORM                                                      
074900       END-IF                                                             
075000                                                                          
075100     END-IF                                                               
075200     .                                                                    
075300     EJECT                                                                
075400 EDB-BEHANDLA-KOLLI-RADER SECTION.                                        
075500                                                                          
075600     PERFORM UNTIL SEGMENT-SAKNAS OR IX > MAX-RADER                       
075700        IF DIST03-SVERIGE-2 AND KOLLI-IDKOLLI > +99000                    
075800           CONTINUE                                                       
075900        ELSE                                                              
076000           PERFORM S01-FLYTTA-KOLLI-RADER                                 
076100           ADD +1 TO IX                                                   
076200        END-IF                                                            
076300        PERFORM IMS-GNP-E611-KOLLI                                        
076400     END-PERFORM                                                          
076500                                                                          
076600     IF SEGMENT-FINNS                                                     
076700        MOVE FEL-X5              TO RESP-IDMSG-INFO                       
076800                                                                          
076900     END-IF                                                               
077000     .                                                                    
077100     EJECT                                                                
077200 EE-VISA-BILD SECTION.                                                    
077300                                                                          
077400     MOVE SPAR-KVKOLLI                 TO RESP-KVKOLLI                    
077500     MOVE SPAR-KVKOLLI-FAKT            TO RESP-KVKOLLI-FAKT               
077600     MOVE SPAR-KVKOLLI-LAST            TO RESP-KVKOLLI-LAST               
077700     MOVE SPAR-KVORDRAD                TO RESP-KVORDRAD-TOT               
077800                                                                          
077900     IF SEC-KDSVAR = 2 OR 6                                               
078000        MOVE ZERO                      TO RESP-SUORDV                     
078100     ELSE                                                                 
078200        IF DIST79-DEALER-PRICE                                            
078300          COMPUTE HELP-SUMMA = SPAR-SUORDV-LOC +                          
078400                               SPAR-SUORDV-LOCPREL                        
078500          MOVE HELP-SUMMA              TO RESP-SUORDV                     
078600          IF SPAR-SUORDV-LOCPREL = +0                                     
078700            MOVE ' '                   TO RESP-TEASTRIX                   
078800          ELSE                                                            
078900            MOVE '*'                   TO RESP-TEASTRIX                   
079000          END-IF                                                          
079100        ELSE                                                              
079200          MOVE SPAR-SUORDV             TO RESP-SUORDV                     
079300          MOVE ' '                     TO RESP-TEASTRIX                   
079400        END-IF                                                            
079500     END-IF                                                               
079600                                                                          
079700     MOVE SPAR-VKORDBTO      TO WS-VKORDBTO                               
079800     MOVE SPAR-VLORDBTO      TO WS-VLORDBTO                               
079900                                                                          
080000     IF US-MEASUREMENT                                                    
080100       COMPUTE WS-VLORDBTO ROUNDED =                                      
080200               WS-VLORDBTO * CONV-M3-TO-FT3 END-COMPUTE                   
080300       COMPUTE WS-VKORDBTO ROUNDED =                                      
080400               WS-VKORDBTO * CONV-KG-TO-LB  END-COMPUTE                   
080500     END-IF                                                               
080600                                                                          
080700     MOVE WS-VLORDBTO        TO RESP-VLORDBTO                             
080800     MOVE WS-VKORDBTO        TO RESP-VKORDBTO                             
080900     .                                                                    
081000     EJECT                                                                
081100 EF-VISA-ORDER-SAKNAS SECTION.                                            
081200                                                                          
081300     MOVE 'IDORDNR'           TO RESP-IDELMT-ERROR                        
081400     MOVE '025'               TO RESP-IDMSG-ERROR                         
081500                                                                          
081600     PERFORM UNTIL IX > MAX-RADER                                         
081700        PERFORM S02-BLANKA-RADER                                          
081800        ADD +1 TO IX                                                      
081900     END-PERFORM                                                          
082000     .                                                                    
082100     EJECT                                                                
082200 F-SPARA-NYCKLAR SECTION.                                                 
082300                                                                          
082400       IF IDPRODNR-WS  NUMERIC                                            
082500        MOVE IDPRODNR-WS             TO RESP-IDPRODNR-KEY                 
082600        INSPECT RESP-IDPRODNR-KEY REPLACING LEADING ZERO BY SPACE         
082700       END-IF                                                             
082800                                                                          
082900        IF REQU-IDDISTR-KEY NUMERIC                                       
083000         MOVE REQU-IDDISTR-KEY           TO RESP-IDDISTR-KEY              
083100         INSPECT RESP-IDDISTR-KEY REPLACING LEADING ZERO BY SPACE         
083200        END-IF                                                            
083300                                                                          
083400        IF REQU-IDKUNDNR-KEY NUMERIC                                      
083500         MOVE REQU-IDKUNDNR-KEY          TO RESP-IDKUNDNR-KEY             
083600         INSPECT RESP-IDKUNDNR-KEY REPLACING LEADING ZERO BY SPACE        
083700        END-IF                                                            
083800                                                                          
083900        IF REQU-IDORDNR7-KEY NUMERIC                                      
084000         MOVE REQU-IDORDNR7-KEY          TO RESP-IDORDNR7-KEY             
084100         INSPECT RESP-IDORDNR7-KEY REPLACING LEADING ZERO BY SPACE        
084200        END-IF                                                            
084300                                                                          
084400     IF IDKOLLI-WS NUMERIC                                                
084500     MOVE IDKOLLI-WS                 TO RESP-IDKOLLI-KEY                  
084600     INSPECT RESP-IDKOLLI-KEY REPLACING LEADING ZERO BY SPACE             
084700     END-IF                                                               
084800                                                                          
084900     IF IDARTNR-WS NUMERIC                                                
085000     MOVE IDARTNR-WS                 TO RESP-IDARTNR-KEY                  
085100     INSPECT RESP-IDARTNR-KEY REPLACING LEADING ZERO BY SPACE             
085200     END-IF                                                               
085300                                                                          
085400     .                                                                    
085500     EJECT                                                                
085600 S01-FLYTTA-KOLLI-RADER SECTION.                                          
085700                                                                          
085800     ADD              +1               TO WS-COUNT                        
085900     IF IDKOLLI-WS > ZERO                                                 
086000       MOVE KOLLI-IDSHIPM              TO RESP-IDSHIPM                    
086100     ELSE                                                                 
086200       MOVE ZERO                       TO RESP-IDSHIPM                    
086300     END-IF                                                               
086400                                                                          
086500     MOVE KOLLI-ADFLGEO                TO RESP-ADFLGEO (IX)               
086600     MOVE KOLLI-ADFLOMR                TO ADFLOMR2-4                      
086700     MOVE ADFLOMR                      TO RESP-ADFLOMR (IX)               
086800     MOVE KOLLI-ADRUTNIV               TO RESP-ADRUTNIV (IX)              
086900     MOVE KOLLI-ADVMODUL               TO RESP-ADVMODUL (IX)              
087000     MOVE KOLLI-KDKOLLI                TO RESP-KDKOLLI (IX)               
087100     MOVE KOLLI-IDPLOCK                TO RESP-IDPLOCK (IX)               
087200     IF AVERAGECOST AND BOUNCEORDER                                       
087300        MOVE KOLLI-TIFAKT-EXP          TO RESP-TIFAKT (IX)                
087400     ELSE                                                                 
087500        MOVE KOLLI-TIFAKT              TO RESP-TIFAKT (IX)                
087600     END-IF                                                               
087700     MOVE KOLLI-TILASTN                TO RESP-TILASTN (IX)               
087800     IF KOLLI-KDKOLSTA = 0 OR KOLLI-DIKOLLIL = 0                          
087810       MOVE ZERO                       TO RESP-TIPACKN (IX)               
087811     ELSE                                                                 
087820       MOVE KOLLI-TIPACKN              TO RESP-TIPACKN (IX)               
087830     END-IF                                                               
087900     MOVE KOLLI-KVORDRAD               TO RESP-KVORDRAD (IX)              
088000     IF KOLLI-KDFARLIG-KOLLI = 4 OR KOLLI-KDFARLIG-KOLLI = 7              
088100        MOVE 'Y '                      TO RESP-TETEXTX2 (IX)              
088200     ELSE                                                                 
088300        MOVE SPACE                     TO RESP-TETEXTX2 (IX)              
088400     END-IF                                                               
088500     MOVE KOLLI-VKORDBTO-KOLLI         TO WS-VKORDBTO                     
088600     MOVE KOLLI-VLORDBTO-KOLLI         TO WS-VLORDBTO                     
088700                                                                          
088800     IF US-MEASUREMENT                                                    
088900       COMPUTE WS-VLORDBTO ROUNDED =                                      
089000               WS-VLORDBTO * CONV-M3-TO-FT3 END-COMPUTE                   
089100       COMPUTE WS-VKORDBTO ROUNDED =                                      
089200               WS-VKORDBTO * CONV-KG-TO-LB  END-COMPUTE                   
089300     END-IF                                                               
089400     MOVE WS-VLORDBTO                  TO RESP-VLORDBTO-KOLLI (IX)        
089500     MOVE WS-VKORDBTO                  TO RESP-VKORDBTO-KOLLI (IX)        
089600                                                                          
089700     IF WS-COUNT  <  501                                                  
089800       CONTINUE                                                           
089900     ELSE                                                                 
090000       MOVE FEL-X7       TO RESP-IDMSG-ERROR                              
090100     END-IF                                                               
090200     MOVE WS-COUNT                     TO  RESP-KVRADER                   
090300                                                                          
090400       MOVE KOLLI-IDKOLLI              TO RESP-IDKOLLI (IX)               
090500     .                                                                    
090600     EJECT                                                                
090700 S02-BLANKA-RADER SECTION.                                                
090800     MOVE SPACE                     TO RESP-RAD (IX)                      
090900     .                                                                    
091000     EJECT                                                                
091100                                                                          
091200*    --- DISPATCHER SECTIONS                                              
091300 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
091400                                                                          
091500     MOVE 'GETARG'               TO SUB-KDFUNC                            
091600     MOVE 'CARPARTS.LDC.ORDERQUERYCASE'      TO SUB-ADDISPABS             
091700     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
091800                                                                          
091900     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
092000                                                                          
092100     IF SUB-KDRC > 0                                                      
092200       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
092300       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
092400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
092500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
092600     END-IF                                                               
092700     .                                                                    
092800     SKIP3                                                                
092900 S02-RETURN-RESPONSE SECTION.                                             
093000                                                                          
093100     MOVE 'RETURN'                   TO SUB-KDFUNC                        
093200     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
093300                                                                          
093400     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
093500                                                                          
093600     IF SUB-KDRC > 0                                                      
093700       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
093800       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
093900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
094000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
094100     END-IF                                                               
094200     .                                                                    
094300     EJECT                                                                
094400* IMS SEKTIONER                                                           
094500 IMS-GU-E4A1-ORDNR SECTION.                                               
094600     SKIP2                                                                
094700     STRING 'WDE4A1  (WDE4A1KY=>' W-WDE4A1KY-MIN-X                        
094800                    '&WDE4A1KY=<' W-WDE4A1KY-MAX-X ')'                    
094900            DELIMITED BY SIZE INTO SSA1                                   
095000     MOVE '  GE' TO GODK-STATUSKODER                                      
095100     CALL CBLTDLI USING GU WDE4A-PCB DLI-IO-AREA-WDE4A1 SSA1              
095200     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
095300     PERFORM IMS-STATUSKONTROLL                                           
095400     .                                                                    
095500     EJECT                                                                
095600 IMS-GN-E4A1-ORDNR SECTION.                                               
095700     SKIP2                                                                
095800     STRING 'WDE4A1  (WDE4A1KY=>' W-WDE4A1KY-MIN-X                        
095900                    '&WDE4A1KY=<' W-WDE4A1KY-MAX-X ')'                    
096000            DELIMITED BY SIZE INTO SSA1                                   
096100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
096200     CALL CBLTDLI USING GN WDE4A-PCB DLI-IO-AREA-WDE4A1 SSA1              
096300     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
096400     PERFORM IMS-STATUSKONTROLL                                           
096500     .                                                                    
096600     EJECT                                                                
096700 IMS-GU-E601-PRODNR-UNIK SECTION.                                         
096800     SKIP2                                                                
096900     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
097000            DELIMITED BY SIZE INTO SSA1                                   
097100     MOVE '  GE' TO GODK-STATUSKODER                                      
097200     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-AREA-WDE601 SSA1               
097300     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
097400     PERFORM IMS-STATUSKONTROLL                                           
097500     .                                                                    
097600     EJECT                                                                
097700 IMS-GU-E601-PRODNR SECTION.                                              
097800     SKIP2                                                                
097900     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
098000            DELIMITED BY SIZE INTO SSA1                                   
098100     MOVE '  ' TO GODK-STATUSKODER                                        
098200     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-AREA-WDE601 SSA1               
098300     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
098400     PERFORM IMS-STATUSKONTROLL                                           
098500     .                                                                    
098600     EJECT                                                                
098700 IMS-GNP-E611-KOLLI SECTION.                                              
098800     SKIP2                                                                
098900     STRING 'WDE611  (IDKOLLI =>' W-IDKOLLI-MIN-X                         
099000                    '&IDKOLLI =<' W-IDKOLLI-MAX-X ')'                     
099100            DELIMITED BY SIZE INTO SSA1                                   
099200     MOVE '  GE' TO GODK-STATUSKODER                                      
099300     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-AREA-WDE611 SSA1              
099400     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
099500     PERFORM IMS-STATUSKONTROLL                                           
099600     .                                                                    
099700     EJECT                                                                
099800                                                                          
099900 IMS-STATUSKONTROLL SECTION.                                              
100000     SKIP2                                                                
100100     SET STATUS-IX TO 1                                                   
100200     SEARCH GODK-STATUS AT END CALL FELLOG                                
100300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
100400     END-SEARCH                                                           
100500     .                                                                    
