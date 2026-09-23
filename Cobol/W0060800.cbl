000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W0060800.                                                
000400 AUTHOR.         MARGARETA GABRIELSSON.                                   
000500 DATE-WRITTEN.   NOVEMBER 1988.                                           
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET STARTAR DIREKTLEVERANTÖRERS                           
001100*        PRINTRAR I IMS ALTERNATIVT STARTAR                               
001200*        SUBMITPAKETSJOBB, SOM HJÄLPER TILL ATT                           
001300*        SPARKA PÅ PRINTERN I VTAM.                                       
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W0T608                                              
001700*        MID:         W0I60801                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W0O60801                                            
002100*                     W0I70902                                            
002200*    SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP3                                                                
002500 DATA DIVISION.                                                           
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002701                                                                          
002710*    -- CHECKED BY WY2000                                                 
002800 77   PROGRAM-NAMN           VALUE 'W0060800'                             
002900                                 PIC X(8).                                
003000 77  JA                          PIC X       VALUE 'J'.                   
003100 77  NEJ                         PIC X       VALUE 'N'.                   
003200 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
003300 77  RAD-IX                      PIC S9(9)   VALUE +0   COMP SYNC.        
003400 77  MAX-IX-PLUS-1               PIC S9(9)   VALUE +3   COMP SYNC.        
003500 77  MAX-MOD-LAENGD              PIC S9(4)   VALUE +115 COMP SYNC.        
003600 77  MIN-MOD-LAENGD              PIC S9(4)   VALUE +49  COMP SYNC.        
003700 77  WS-IDTRANS                  PIC X(4).                                
003800   88  WS-GODKAEND-BILD                      VALUE '0608'.                
003900     SKIP3                                                                
004000 01  INDATA-SW                   PIC X.                                   
004100   88  INDATA-OK                             VALUE 'J'.                   
004200   88  INDATA-FEL                            VALUE 'N'.                   
004300 01  W-ALT-ISRT                  PIC X       VALUE 'N'.                   
004400 01  W-GODKAEND-USER             PIC X.                                   
004500 01  W-KORREKT-IFYLLT            PIC X       VALUE 'N'.                   
004600 01  W-VALD-PRINTER              PIC X(3)    VALUE SPACE.                 
004700 01  W-IDLTERM-NAMN              PIC X(8)    VALUE SPACE.                 
004800 01  W-IMS-PRINTER-OPEN          PIC X       VALUE 'N'.                   
004900 01  W-IDRUTIN                   PIC X(8)    VALUE 'W400S2'.              
005000     EJECT                                                                
005100 01  DYNAMISKA-SUBPROGRAM.                                                
005200     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
005210     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005220     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005300     SKIP3                                                                
005310                                                                          
005400 01  W-IDUSER.                                                            
005500     03  W-IDUSER-POS-1-6.                                                
005600         05  W-IDUSER-POS-1-4    PIC X(4)    VALUE SPACE.                 
005700         05  W-IDUSER-POS-5      PIC X(1)    VALUE SPACE.                 
005800         05  W-IDUSER-POS-6      PIC X(1)    VALUE SPACE.                 
005900     03  FILLER                  PIC X(2)    VALUE SPACE.                 
006000     EJECT                                                                
006100 01  USER-TABELL.                                                         
006200     03  IDUSER-TABELL.                                                   
006500       05  FILLER        PIC X(17) VALUE 'PCSSE2456GOODYEAR'.             
006700       05  FILLER        PIC X(17) VALUE 'PCSSE4454HERBERTS'.             
006800       05  FILLER        PIC X(17) VALUE 'PCSSE5458TORSMASK'.             
006900       05  FILLER        PIC X(17) VALUE 'PCSSE7453DUNLOP  '.             
006910       05  FILLER        PIC X(17) VALUE 'PCSSE9460PIRELLI '.             
007000       05  FILLER        PIC X(17) VALUE 'PC5970001MAGGAN  '.             
007200       05  FILLER        PIC X(17) VALUE 'RS00L2456GOODYEAR'.             
007300       05  FILLER        PIC X(17) VALUE 'RS00L4454HERBERTS'.             
007310       05  FILLER        PIC X(17) VALUE 'RS00L5458TORSMASK'.             
007320       05  FILLER        PIC X(17) VALUE 'RS00L7453DUNLOP  '.             
007330       05  FILLER        PIC X(17) VALUE 'RS00L9460PIRELLI '.             
007400     SKIP2                                                                
007500     03  REIDUSER REDEFINES IDUSER-TABELL.                                
007600       05 IDUSER-TAB OCCURS 11                                            
007700          ASCENDING KEY TAB-IDUSER                                        
007800          INDEXED BY TAB-IX.                                              
007900         07  TAB-IDUSER           PIC X(6).                               
008000         07  TAB-PRINTER          PIC X(3).                               
008100         07  TAB-IDJOB            PIC X(8).                               
008200     EJECT                                                                
008300 01  COMMANDS.                                                            
008400     03  OPN-LTERM.                                                       
008500         05  FILLER     PIC S9(4)   VALUE +22    COMP SYNC.               
008600         05  FILLER     PIC X(2)    VALUE LOW-VALUE.                      
008700         05  FILLER     PIC X(10)   VALUE '/OPN NODE '.                   
008800         05  LTERM      PIC X(8)    VALUE SPACE.                          
008900     03  OPN-SVAR       PIC X(39)   VALUE 'WDCMD02 COMMAND IN PROG        
009000-                                         'RESS OR EXECUTED'.             
009100     03  OPN-SVAR-2.                                                      
009200         05  OPN-SVAR2-DEL1 PIC X(7)  VALUE 'DFS058 '.                    
009300         05  FILLER         PIC X(52).                                    
009400     EJECT                                                                
009500 01  MEDDELANDE.                                                          
009600   03  FEL1.                                                              
009700     05 FILLER                   PIC X(40)                                
009800          VALUE 'UPPLYSTA FÄLT FEL'.                                      
009900     05 FILLER                   PIC X(40)                                
010000          VALUE 'HIGHLIT FIELDS ARE WRONG'.                               
010100   03  FILLER REDEFINES FEL1.                                             
010200     05  FEL-1                   PIC X(40)   OCCURS 2.                    
010300   03  FEL2.                                                              
010400     05 FILLER                   PIC X(40)                                
010500          VALUE 'DU ÄR INTE TILLÅTEN ATT STARTA PRINTNING'.               
010600     05 FILLER                   PIC X(40)                                
010700          VALUE 'YOU ARE NOT ALLOWED TO START PRINTING'.                  
010800   03  FILLER REDEFINES FEL2.                                             
010900     05  FEL-2                   PIC X(40)   OCCURS 2.                    
011000   03  FEL3.                                                              
011100     05 FILLER                   PIC X(40)                                
011200          VALUE 'PRINTERN ÄR INTE STARTAD FÖR KVANTORDER'.                
011300     05 FILLER                   PIC X(40)                                
011400          VALUE 'PRINTER NOT STARTED FOR BULK ORDERS'.                    
011500   03  FILLER REDEFINES FEL3.                                             
011600     05  FEL-3                   PIC X(40)   OCCURS 2.                    
011700   EJECT                                                                  
011800   03  MED1.                                                              
011900     05 FILLER                   PIC X(61)                                
012000          VALUE 'PRINTERN ÄR STARTAD'.                                    
012100     05 FILLER                   PIC X(61)                                
012200          VALUE 'YOUR PRINTER HAS BEEN STARTED'.                          
012300   03  FILLER REDEFINES MED1.                                             
012400     05  MED-1                   PIC X(61)   OCCURS 2.                    
012500   03  MED2.                                                              
012600     05 FILLER                   PIC X(61)                                
012700          VALUE 'PRINTERN ÄR STARTAD FÖR KVANTORDER'.                     
012800     05 FILLER                   PIC X(61)                                
012900          VALUE 'THE PRINTER HAS BEEN STARTED FOR BULK ORDERS'.           
013000   03  FILLER REDEFINES MED2.                                             
013100     05  MED-2                   PIC X(61)   OCCURS 2.                    
013200   EJECT                                                                  
013300*01  -COPY W006PRT                                                        
013400   EJECT                                                                  
013500******************************************************************        
013600*                                                                         
013700*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
013800*                                                                         
013900 01  FILLER                      PIC X(16)   VALUE 'MFS-WS'.              
014000     SKIP3                                                                
014100*01  MID -COPY W0I60801                                                   
014200     EJECT                                                                
014300*01  -COPY WMSGAREA                                                       
014400     EJECT                                                                
014500*  03  MOD -COPY W0O60801           -RED MSG-AREA.                        
014600     EJECT                                                                
014700*01  MOD -COPY W0I70901       -PRE MOD-.                                  
014800     EJECT                                                                
014900*01  -COPY WMFSAREA                                                       
015000     EJECT                                                                
015100******************************************************************        
015200*                                                                         
015300*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015400*                                                                         
015500 01  IMS-WS.                                                              
015600   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
015700     SKIP3                                                                
015800*                        **** STATUS-KOD FRÅN IMS                         
015900   03  STATUS-WS                 PIC XX.                                  
016000     88  SEGMENT-FINNS                       VALUE '  '.                  
016100     88  STATUS-OK                           VALUE '  '.                  
016200     88  SEGMENT-SAKNAS                      VALUE 'QD'.                  
016300     88  TRANSKOD-FEL                        VALUE 'A1'.                  
016400     88  SECURITY-FEL                        VALUE 'A4'.                  
016500     88  FLERA-SVAR-FINNS                    VALUE 'CC'.                  
016600     SKIP3                                                                
016700   03  GODK-STATUSKODER.                                                  
016800     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016900     SKIP3                                                                
017000 01    SSA1                      PIC X(64).                               
017100     EJECT                                                                
017200*                            IMS FUNKTIONSKODER                           
017300*01    -COPY W0003                                                        
017500     EJECT                                                                
017600*                            CMD-INPUT-OUTPUT AREA                        
017700 01  CMD-IO-AREA.                                                         
017800   03  IO-AREA.                                                           
017900     SKIP2                                                                
018000       05  FILLER           PIC X(8).                                     
018100       05  CMD-IO-SVAR      PIC X(124).                                   
018200       05  CMD-IO-SVAR-1 REDEFINES CMD-IO-SVAR PIC X(39).                 
018300       05  CMD-IO-SVAR-2 REDEFINES CMD-IO-SVAR.                           
018400           07  CMD-IO-SVAR-2-DEL1  PIC X(7).                              
018500           07  FILLER              PIC X(52).                             
018600     EJECT                                                                
018700 LINKAGE SECTION.                                                         
018800*01  -COPY W0009     -PRE MSG-                                            
019000     EJECT                                                                
019100*01  -COPY W0009     -PRE ALT-                                            
019300     EJECT                                                                
019400 PROCEDURE DIVISION USING MSG-PCB ALT-PCB.                                
019500     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB.                               
019600                                                                          
019700     PERFORM IMS-GET-MSG                                                  
019800     IF SEGMENT-FINNS                                                     
019900       PERFORM A-INIT                                                     
020000                                                                          
020100       IF WS-GODKAEND-BILD                                                
020200         PERFORM B-KOLLA-INDATA                                           
020300         IF W-KORREKT-IFYLLT = JA                                         
020400           PERFORM C-SPARKA-IGANG-PRINTER                                 
020500                                                                          
020600           IF W-GODKAEND-USER = NEJ                                       
020700             MOVE FEL-2 (SPRAK-IX) TO MOD-TEMFSFEL                        
020800             PERFORM MFS-ROER-EJ                                          
020900           END-IF                                                         
021000                                                                          
021100         ELSE                                                             
021200           IF RAD-IX = +1 OR +2                                           
021300             MOVE FEL-1 (SPRAK-IX) TO MOD-TEMFSFEL                        
021400             PERFORM MFS-ROER-EJ                                          
021500           END-IF                                                         
021600         END-IF                                                           
021700       END-IF                                                             
021800                                                                          
021900       IF W-ALT-ISRT = NEJ                                                
022000         MOVE MAX-MOD-LAENGD TO MSG-KVLL                                  
022100         PERFORM IMS-INSERT-MSG                                           
022200       END-IF                                                             
022300     END-IF                                                               
022400                                                                          
022500     MOVE ZERO TO RETURN-CODE                                             
022600     GOBACK                                                               
022700                                                                          
022800     .                                                                    
022900     EJECT                                                                
023000 A-INIT SECTION.                                                          
023100                                                                          
023200     IF MSG-DUBBLA-TRANSKODER                                             
023300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I60801                 
023400       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS WS-IDTRANS                       
023500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
023600       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
023700       MOVE MSG-IDPFK TO MFS-IDPFK                                        
023800     ELSE                                                                 
023900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W0I60801                  
024000       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS WS-IDTRANS                       
024100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
024200       MOVE SPACE TO MFS-KDTRTYP        MFS-IDPFK                         
024300     END-IF                                                               
024400                                                                          
024500     MOVE MSG-SIGNON-USERID TO W-IDUSER                                   
024600                                                                          
024700     MOVE LOW-VALUE TO MSG-AREA                                           
024800     MOVE 'W0O60801' TO MFS-IDMOD                                         
024900     MOVE '0608' TO MOD-IDTRANS                                           
025000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
025100                                                                          
025200     IF MFS-KDMFSFOR = '2'                                                
025300       MOVE +2 TO SPRAK-IX                                                
025400     ELSE                                                                 
025500       MOVE +1 TO SPRAK-IX                                                
025600     END-IF                                                               
025700                                                                          
025800     MOVE +1 TO RAD-IX                                                    
025900     PERFORM UNTIL RAD-IX = MAX-IX-PLUS-1                                 
026000       MOVE MFS-RENSA-FAELT TO MOD-KDCMD (RAD-IX)                         
026100       ADD +1 TO RAD-IX                                                   
026200     END-PERFORM                                                          
026300                                                                          
026400     MOVE NEJ TO W-GODKAEND-USER                                          
026500     MOVE NEJ TO W-KORREKT-IFYLLT                                         
026600     MOVE NEJ TO W-ALT-ISRT                                               
026700     .                                                                    
026800     EJECT                                                                
026900 B-KOLLA-INDATA SECTION.                                                  
027000                                                                          
027100     MOVE +1 TO RAD-IX                                                    
027200                                                                          
027300     PERFORM UNTIL RAD-IX = MAX-IX-PLUS-1 OR                              
027400        W-KORREKT-IFYLLT = JA                                             
027500         IF MID-KDCMD (RAD-IX) = '+'                                      
027600           MOVE MFS-ALFA-FAELT-RAETT TO                                   
027700                           MOD-KDCMD-ATTR (RAD-IX)                        
027800         ELSE                                                             
027900           IF MID-KDCMD (RAD-IX) = 'X'                                    
028000             MOVE JA  TO W-KORREKT-IFYLLT                                 
028100             MOVE MFS-ALFA-FAELT-RAETT TO                                 
028200                                 MOD-KDCMD-ATTR (RAD-IX)                  
028300           ELSE                                                           
028400             MOVE MFS-ALFA-FAELT-FEL TO                                   
028500                                 MOD-KDCMD-ATTR (RAD-IX)                  
028600           END-IF                                                         
028700         END-IF                                                           
028800       ADD +1 TO RAD-IX                                                   
028900     END-PERFORM                                                          
029000                                                                          
029100     IF W-KORREKT-IFYLLT = JA                                             
029200       IF RAD-IX = +2                                                     
029300         MOVE +1 TO RAD-IX                                                
029400       ELSE                                                               
029500         MOVE +2 TO RAD-IX                                                
029600       END-IF                                                             
029700     END-IF                                                               
029800     .                                                                    
029900     EJECT                                                                
030000 C-SPARKA-IGANG-PRINTER.                                                  
030100     SKIP2                                                                
030200     IF RAD-IX = +1                                                       
030300       PERFORM CA-OPEN-KOMMANDO                                           
030400     ELSE                                                                 
030410       PERFORM CA-OPEN-KOMMANDO                                           
030600     END-IF                                                               
030700                                                                          
030800     .                                                                    
030900     SKIP3                                                                
031000 CA-OPEN-KOMMANDO SECTION.                                                
031100     SKIP2                                                                
031200     SET TAB-IX TO 1                                                      
031300     SEARCH ALL IDUSER-TAB AT END                                         
031400       MOVE NEJ TO W-GODKAEND-USER                                        
031500       WHEN TAB-IDUSER (TAB-IX) = W-IDUSER-POS-1-6                        
031600         MOVE TAB-PRINTER (TAB-IX) TO W-VALD-PRINTER                      
031700         MOVE JA TO W-GODKAEND-USER                                       
031800     END-SEARCH                                                           
031900                                                                          
032000     IF W-GODKAEND-USER = JA                                              
032100       MOVE SPACE          TO PRT-IDPRTLST                                
032200       MOVE W-VALD-PRINTER TO PRT-IDPRTLST                                
032300       MOVE 1              TO PRT-KDCALL                                  
032400       CALL W006PRT  USING PRT-W006PRT                                    
032500       MOVE PRT-IDLTERM    TO LTERM                                       
032600                                                                          
032700       MOVE OPN-LTERM TO CMD-IO-AREA                                      
032800       PERFORM IMS-INSERT-CMD                                             
032900       MOVE STATUS-WS TO MOD-TEMFSINF                                     
033000       IF FLERA-SVAR-FINNS                                                
033100         MOVE SPACE TO STATUS-WS                                          
033200         PERFORM UNTIL SEGMENT-SAKNAS                                     
033300           IF OPN-SVAR = CMD-IO-SVAR OR                                   
033400              OPN-SVAR2-DEL1 = CMD-IO-SVAR-2-DEL1                         
033500                 MOVE MED-1 (SPRAK-IX) TO MOD-TEMFSINF                    
033600           ELSE                                                           
033700              MOVE MED-1 (SPRAK-IX) TO MOD-TEMFSINF                       
033800           END-IF                                                         
033900           MOVE SPACE TO CMD-IO-AREA                                      
034000           PERFORM IMS-GET-GCMD                                           
034100         END-PERFORM                                                      
034200       ELSE                                                               
034300         MOVE MED-1 (SPRAK-IX) TO MOD-TEMFSINF                            
034400       END-IF                                                             
034500     END-IF                                                               
034600                                                                          
034700     .                                                                    
034800     EJECT                                                                
037400 MFS-ROER-EJ SECTION.                                                     
037500                                                                          
037600     MOVE +1 TO RAD-IX                                                    
037700                                                                          
037800     PERFORM UNTIL RAD-IX = MAX-IX-PLUS-1                                 
037900                                                                          
038000       MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD (RAD-IX)                       
038100       ADD +1 TO RAD-IX                                                   
038200                                                                          
038300     END-PERFORM                                                          
038400                                                                          
038500     .                                                                    
038600     EJECT                                                                
038700* IMS SEKTIONER                                                           
038800     SKIP3                                                                
038900 IMS-GET-MSG SECTION.                                                     
039000                                                                          
039100     MOVE '  QC' TO GODK-STATUSKODER                                      
039200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
039300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
039400     PERFORM IMS-STATUSKONTROLL                                           
039500     SKIP3                                                                
039600                                                                          
039700     .                                                                    
040700 IMS-INSERT-CMD SECTION.                                                  
040800                                                                          
040900     MOVE '  CC' TO GODK-STATUSKODER                                      
041000     CALL CBLTDLI USING CMD MSG-PCB CMD-IO-AREA                           
041100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
041200     PERFORM IMS-STATUSKONTROLL                                           
041300     EJECT                                                                
041400     .                                                                    
041500 IMS-INSERT-MSG SECTION.                                                  
041600                                                                          
041700     IF ENGLISH-TEXT                                                      
041800       MOVE 'N' TO MFS-KDHUVOMR                                           
041900     END-IF                                                               
042000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
042100     MOVE SPACE TO GODK-STATUSKODER                                       
042200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
042300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
042400     PERFORM IMS-STATUSKONTROLL                                           
042500     EJECT                                                                
042600     .                                                                    
042700 IMS-GET-GCMD SECTION.                                                    
042800                                                                          
042900     MOVE '  QD' TO GODK-STATUSKODER                                      
043000     CALL CBLTDLI USING GCMD MSG-PCB CMD-IO-AREA                          
043100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
043200     PERFORM IMS-STATUSKONTROLL                                           
043300     EJECT                                                                
043400     .                                                                    
043500 IMS-STATUSKONTROLL SECTION.                                              
043600                                                                          
043700     SET STATUS-IX TO 1                                                   
043800     SEARCH GODK-STATUS                                                   
043810       AT END                                                             
043820         CALL FELLOG                                                      
043900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
044000     END-SEARCH                                                           
044100                                                                          
044200     .                                                                    
