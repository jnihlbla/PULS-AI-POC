000400 ID DIVISION.                                                             
000500     SKIP2                                                                
000600 PROGRAM-ID.     W1050500.                                                
001000*AUTHOR.         BODIL LINDAHL.                                           
001100*DATE-WRITTEN.   MAJ 1983.                                                
001200                                                                          
001400*                                                                         
001500*    FUNKTION.                                                            
001600*        SÖKNING TILLÄGGSTEXT.                                            
001700*                                                                         
001800*                                                                         
001900*                                                                         
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSAKTION: W1T505                                              
002300**       MID:         W1I50501                                            
002400**                                                                        
002500*    UTDATA.                                                              
002600*        MOD:         W1O50501                                            
002700*                                                                         
002800*    SUBPROGRAM.                                                          
002900*        CBLTDLI                                                          
003000*        FELLOG                                                           
003100*        W009LTXT                                                         
003200*                                                                         
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     SKIP3                                                                
003600 DATA DIVISION.                                                           
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003810                                                                          
003900*    -- CHECKED BY WY2000                                                 
004000 77  IDPGM                       PIC X(8)    VALUE 'W1050500'.            
004600 77    JA                        PIC X       VALUE 'J'.                   
004700 77    NEJ                       PIC X       VALUE 'N'.                   
004800 77    SVENSK                    PIC X(3)    VALUE 'S  '.                 
004900 77    WS-BETTEXT                PIC X(25)   VALUE SPACE.                 
005200 77    WS-IDTTEXNR               PIC X(5)    VALUE SPACE.                 
005300 77    WS-IDSKYLT                PIC X(3)    VALUE SPACE.                 
005400 77    WS-IDTTEXNR-SPAR          PIC 9(5)    VALUE ZERO.                  
005500 77    NYCKLAR-RETT              PIC X       VALUE 'J'.                   
005600 77    LAS-VIDARE                PIC X       VALUE 'J'.                   
005700 77    INDX                      PIC S9(9)   VALUE +0   COMP SYNC.        
005800 77    RAD-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
005900 77    MAX-RADER                 PIC S9(3)   VALUE +14.                   
006000 77    MAX-RADER-PLUS-1          PIC S9(3)   VALUE +15.                   
006100 77    MAX-MOD-LAENGD            PIC S9(4)  VALUE +1228 COMP SYNC.        
006110 77    FAELT-LAENGD              PIC S9(4)  VALUE +25 COMP.               
006200     SKIP2                                                                
006300 01    WS-IDTRANS                PIC X(4).                                
006400       88  EGEN-BILD             VALUE '1505'.                            
006500       88  GODKAEND-BILD         VALUE '1501' '1502' '1503'               
006600                                       '1505' '1506'                      
006700                                       '1508' '1509'.                     
006800     EJECT                                                                
006900 01    LITEN-BOKSTAV        PIC X(28)                                     
007000                  VALUE 'abcdefghijklmnopqrstuvxyzåäö'.                   
007100     SKIP3                                                                
007200 01    STOR-BOKSTAV         PIC X(28)                                     
007300                  VALUE 'ABCDEFGHIJKLMNOPQRSTUVXYZÅÄÖ'.                   
007400     SKIP3                                                                
007500 01    DYNAMISKA-SUBPROGRAM.                                              
007600       03  W009LTXT         PIC X(8)  VALUE 'W009LTXT'.                   
007630       03  CBLTDLI          PIC X(8)  VALUE 'CBLTDLI '.                   
007640       03  FELLOG           PIC X(8)  VALUE 'FELLOG  '.                   
007700     EJECT                                                                
007800*01    -COPY WWLAND03                                                     
008000     EJECT                                                                
008100*01             -COPY W009W041                                            
008300     EJECT                                                                
008400 01    NYCKLAR-TILL-DLI.                                                  
008500   03    W-IDSKYLT-X.                                                     
008600     05    W-IDSKYLT             PIC X(3)    VALUE SPACE.                 
008900   03    W-IDTTEXNR-X.                                                    
009000     05    W-IDTTEXNR            PIC S9(5)   VALUE ZERO  COMP-3.          
009100   03    W-IDTTEXNR-MAX-X.                                                
009200     05    FILLER                PIC S9(5)   VALUE +99999 COMP-3.         
009300     SKIP3                                                                
009400 01  MEDDELANDE.                                                          
009500*                                                                         
009600   03 MEDD-1.                                                             
009700      05 FILLER                  PIC X(24)   VALUE                        
009800             'TRYCK PF8 FÖR FLER RADER'.                                  
009900      05 FILLER                  PIC X(24)   VALUE                        
010000             'PRESS PF8 FOR MORE LINES'.                                  
010100   03 FILLER    REDEFINES MEDD-1.                                         
010200      05 MED-1 OCCURS 2          PIC X(24).                               
010300*                                                                         
010400   03 MEDD-2.                                                             
010500      05 FILLER                  PIC X(19)   VALUE                        
010600             'FÖRSTA SIDAN VISAS '.                                       
010700      05 FILLER                  PIC X(19)   VALUE                        
010800             'FIRST PAGE IS SHOWN'.                                       
010900   03 FILLER    REDEFINES MEDD-2.                                         
011000      05 MED-2 OCCURS 2          PIC X(19).                               
011100*                                                                         
011200   03 FEL-01.                                                             
011300      05 FILLER                  PIC X(12)   VALUE                        
011400             'NYCKLAR FEL '.                                              
011500      05 FILLER                  PIC X(12)   VALUE                        
011600             'WRONG KEY(S)'.                                              
011700   03 FILLER    REDEFINES FEL-01.                                         
011800      05 FEL-1 OCCURS 2          PIC X(12).                               
011900*                                                                         
012000   03 FEL-02.                                                             
012100      05 FILLER                  PIC X(20)   VALUE                        
012200             'MATA IN NYCKLAR     '.                                      
012300      05 FILLER                  PIC X(20)   VALUE                        
012400             'PLEASE, TYPE IN KEYS'.                                      
012500   03 FILLER    REDEFINES FEL-02.                                         
012600      05 FEL-2 OCCURS 2          PIC X(20).                               
012700*                                                                         
012800   03 FEL-03.                                                             
012900      05 FILLER                  PIC X(15)   VALUE                        
013000             'TEXT SAKNAS    '.                                           
013100      05 FILLER                  PIC X(15)   VALUE                        
013200             'TEXT IS MISSING'.                                           
013300   03 FILLER    REDEFINES FEL-03.                                         
013400      05 FEL-3 OCCURS 2          PIC X(15).                               
013500*                                                                         
013600     EJECT                                                                
013700******************************************************************        
013800*                                                                         
013900*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
014000*                                                                         
014100 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
014200     SKIP3                                                                
014300*01    MID -COPY W1I50501          .                                      
014500     EJECT                                                                
014600*01    -COPY WMSGAREA                                                     
014800     EJECT                                                                
014900*  03    MOD -COPY W1O50501           -RED MSG-AREA.                      
015100     EJECT                                                                
015200*01    -COPY WMFSAREA                                                     
015400     EJECT                                                                
015500******************************************************************        
015600*                                                                         
015700*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015800*                                                                         
015900 01    IMS-WS.                                                            
016000   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
016100     SKIP3                                                                
016200*                        **** STATUS-KOD FRÅN IMS                         
016300   03    STATUS-WS               PIC XX.                                  
016400     88    SEGMENT-FINNS                     VALUE '  '.                  
016500     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
016600     88    BASEN-SLUT                        VALUE 'GB'.                  
016700     SKIP3                                                                
016800   03    GODK-STATUSKODER.                                                
016900     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
017000     SKIP3                                                                
017100 01    SSA1                      PIC X(120).                              
017200     EJECT                                                                
017300*                            IMS FUNKTIONSKODER                           
017400*01    -COPY W0003                                                        
017600     EJECT                                                                
017700*                            DLI INPUT-OUTPUT AREA                        
017800 01    DLI-IO-AREA.                                                       
017900   03    IO-AREA                 PIC X(200)  VALUE SPACE.                 
018000     SKIP3                                                                
018100*  03    WLKATD01 -COPY WDN401  -PRE KATD-   -RED IO-AREA.                
018300     EJECT                                                                
018400*  03    WLKATD11 -COPY WDN411  -PRE KATD-   -RED IO-AREA.                
018600     EJECT                                                                
018700*  03    WLKATE01 -COPY WDN4A1  -PRE KATE-   -RED IO-AREA.                
018900     EJECT                                                                
019000 LINKAGE SECTION.                                                         
019100*01    -COPY W0009     -PRE MSG-                                          
019300     EJECT                                                                
019400*01    -COPY W0008     -PRE KATD-                                         
019600     05  FILLER                  PIC X.                                   
019700     EJECT                                                                
019800*01    -COPY W0008     -PRE KATE-                                         
020000     05  FILLER                  PIC X.                                   
020100     EJECT                                                                
020200 PROCEDURE DIVISION USING MSG-PCB KATD-PCB KATE-PCB.                      
020300     ENTRY 'DLITCBL' USING MSG-PCB KATD-PCB KATE-PCB.                     
020400     SKIP2                                                                
020500     PERFORM IMS-GET-MSG                                                  
020600     IF SEGMENT-FINNS                                                     
020700       PERFORM A-INIT-SPARA-INPUT                                         
020800       IF GODKAEND-BILD                                                   
020900         PERFORM B-KOLLA-NYCKLAR                                          
021000         IF NYCKLAR-RETT = JA                                             
021100           IF EGEN-BILD                                                   
021200             PERFORM D-KOLLA-PFTANGENTER                                  
021300           ELSE                                                           
021400             MOVE ZERO TO WS-IDTTEXNR-SPAR                                
021500           END-IF                                                         
021600           PERFORM C-LAS-BASEN                                            
021700         ELSE                                                             
021800           MOVE FEL-1(INDX) TO MOD-TEMFSFEL                               
021900         END-IF                                                           
022000       ELSE                                                               
022100         MOVE FEL-2(INDX) TO MOD-TEMFSFEL                                 
022200         MOVE SPACE TO WS-BETTEXT                                         
022300                       WS-IDSKYLT                                         
022600         MOVE MFS-RENSA-FAELT TO MOD-IDTTEXNR-SPAR                        
022700       END-IF                                                             
022800       MOVE WS-BETTEXT TO MOD-BETTEXT-SOEK-UT                             
022900       MOVE WS-IDSKYLT TO MOD-IDSKYLT-UT                                  
023200       MOVE WS-IDTTEXNR TO MOD-IDTTEXNR-UT                                
023300       INSPECT MOD-IDTTEXNR-UT REPLACING LEADING ZERO BY SPACE            
023400                                                                          
023500       IF WS-IDTTEXNR-SPAR = ZERO                                         
023600         MOVE MFS-RENSA-FAELT TO MOD-IDTTEXNR-SPAR                        
023700       ELSE                                                               
023800         MOVE WS-IDTTEXNR-SPAR TO MOD-IDTTEXNR-SPAR                       
023900       END-IF                                                             
024000       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
024100       PERFORM IMS-INSERT-MSG                                             
024200     END-IF                                                               
024300     MOVE ZERO TO RETURN-CODE                                             
024400     GOBACK                                                               
024500     CONTINUE.                                                            
024600     EJECT                                                                
024700 A-INIT-SPARA-INPUT SECTION.                                              
024800     SKIP2                                                                
024900     IF MSG-DUBBLA-TRANSKODER                                             
025000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I50501                 
025100       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
025200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
025300       MOVE MSG-IDPFK TO MFS-IDPFK                                        
025400     ELSE                                                                 
025500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I50501                  
025600       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
025700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
025800       MOVE ' ' TO MFS-IDPFK                                              
025900     END-IF                                                               
026000     MOVE MFS-IDTRANS TO WS-IDTRANS                                       
026100                                                                          
026200                                                                          
026300     MOVE LOW-VALUE TO MSG-AREA                                           
026400     MOVE 'W1O50501' TO MFS-IDMOD                                         
026500     MOVE '1505' TO MOD-IDTRANS                                           
026600     IF SWEDISH-TEXT                                                      
026700       MOVE +1 TO INDX                                                    
026800     ELSE                                                                 
026900       MOVE +2 TO INDX                                                    
027000     END-IF                                                               
027100     MOVE MFS-RENSA-FAELT TO MOD-BETTEXT-SOEK-IN                          
027200                             MOD-IDSKYLT-IN                               
027500                             MOD-IDTTEXNR-IN                              
027600                             MOD-TEMFSFEL                                 
027700                             MOD-TEMFSINF                                 
027800     CONTINUE.                                                            
027900     EJECT                                                                
028000 B-KOLLA-NYCKLAR SECTION.                                                 
028100     SKIP2                                                                
028200                                                                          
028300     MOVE JA TO NYCKLAR-RETT                                              
028400                                                                          
028500     IF MID-IDSKYLT-IN = ALL '+'                                          
028600       IF MID-IDSKYLT-UT = SPACE                                          
028700         MOVE SVENSK TO WS-IDSKYLT                                        
028800         MOVE '7' TO MFS-IDPFK                                            
028900       ELSE                                                               
029000         MOVE MID-IDSKYLT-UT TO WS-IDSKYLT                                
029100       END-IF                                                             
029200     ELSE                                                                 
029300       MOVE MID-IDSKYLT-IN TO WS-IDSKYLT                                  
029400       MOVE '7' TO MFS-IDPFK                                              
029500     END-IF                                                               
029700     INSPECT WS-IDSKYLT CONVERTING LITEN-BOKSTAV TO STOR-BOKSTAV          
029800     SET WWLAND03-IX TO +1                                                
029900     SEARCH WWLAND03-IDSKYLT-RAD                                          
030000            AT END MOVE NEJ TO NYCKLAR-RETT                               
030100            WHEN WWLAND03-IDSKYLT(WWLAND03-IX) = WS-IDSKYLT               
030200            MOVE JA TO NYCKLAR-RETT                                       
030300     END-SEARCH                                                           
030400                                                                          
030500     IF MID-BETTEXT-SOEK-IN = ALL '+'                                     
030600       MOVE MID-BETTEXT-SOEK-UT TO WS-BETTEXT                             
030700     ELSE                                                                 
030800       MOVE MID-BETTEXT-SOEK-IN TO WS-BETTEXT                             
030900       MOVE '7' TO MFS-IDPFK                                              
031000     END-IF                                                               
031100     IF WS-BETTEXT = SPACE                                                
031200       MOVE NEJ TO NYCKLAR-RETT                                           
031300     END-IF                                                               
032700     IF MID-IDTTEXNR-IN = ALL '+'                                         
032800       MOVE MID-IDTTEXNR-UT TO WS-IDTTEXNR                                
032900     ELSE                                                                 
033000       MOVE MID-IDTTEXNR-IN TO WS-IDTTEXNR                                
033100       INSPECT WS-IDTTEXNR REPLACING LEADING SPACE BY ZERO                
033200     END-IF                                                               
033300     CONTINUE.                                                            
033400     EJECT                                                                
033500 C-LAS-BASEN SECTION.                                                     
033600     SKIP2                                                                
033700     MOVE JA TO LAS-VIDARE                                                
033800     MOVE 1 TO RAD-IX                                                     
033900                                                                          
034000     MOVE WS-IDTTEXNR-SPAR TO W-IDTTEXNR                                  
034100     MOVE WS-IDSKYLT TO W-IDSKYLT                                         
034200                                                                          
034300     PERFORM IMS-GET-KATE01                                               
034400     PERFORM UNTIL                                                        
034500      NOT ( SEGMENT-FINNS AND LAS-VIDARE = JA )                           
034600       MOVE WS-BETTEXT TO W041-BESORD                                     
034700       MOVE KATE-TTEXTA-BETTEXT TO W041-BESTEXT                           
034710       MOVE FAELT-LAENGD TO W041-DIFAELT                                  
034800       MOVE KATE-TTEXTA-IDTTEXNR TO W-IDTTEXNR                            
034900       CALL W009LTXT USING W041-W009W041                                  
035000       IF W041-OK                                                         
035100         IF RAD-IX = MAX-RADER-PLUS-1                                     
035200           MOVE KATE-TTEXTA-IDTTEXNR TO WS-IDTTEXNR-SPAR                  
035300           MOVE MED-1(INDX) TO MOD-TEMFSINF                               
035400           MOVE NEJ TO LAS-VIDARE                                         
035500         ELSE                                                             
035600           PERFORM IMS-GET-KATD01                                         
035700           MOVE KATD-TTEXT-IDTTEXNR TO MOD-IDTTEXNR(RAD-IX)               
035800           PERFORM IMS-GET-KATD11                                         
035900           PERFORM UNTIL                                                  
036000            NOT ( SEGMENT-FINNS )                                         
036100             PERFORM CA-FLYTTA-TILL-RAD                                   
036200             PERFORM IMS-GET-KATD11                                       
036300           END-PERFORM                                                    
036400           ADD +1 TO RAD-IX                                               
036500           MOVE ZERO TO WS-IDTTEXNR-SPAR                                  
036600           PERFORM IMS-GET-KATE01                                         
036700         END-IF                                                           
036800       ELSE                                                               
036900         PERFORM IMS-GET-KATE01                                           
037000       END-IF                                                             
037100     END-PERFORM                                                          
037200     IF SEGMENT-SAKNAS OR BASEN-SLUT                                      
037300       IF RAD-IX = 1                                                      
037400         MOVE FEL-3(INDX) TO MOD-TEMFSFEL                                 
037500         MOVE ZERO TO WS-IDTTEXNR-SPAR                                    
037600       END-IF                                                             
037700     END-IF                                                               
037800     CONTINUE.                                                            
037900     EJECT                                                                
038000  CA-FLYTTA-TILL-RAD SECTION.                                             
038100     SKIP2                                                                
038200     IF WS-IDSKYLT = KATD-TEXT-IDSKYLT                                    
038300       MOVE KATD-TEXT-BETTEXT TO MOD-BETTEXT(RAD-IX)                      
038400     ELSE                                                                 
038500       EVALUATE TRUE                                                      
038600       WHEN KATD-TEXT-IDSKYLT = 'S  '                                     
038700         MOVE KATD-TEXT-IDSKYLT TO MOD-IDSKYLT(RAD-IX 1)                  
038800       WHEN KATD-TEXT-IDSKYLT = 'GB '                                     
038900         MOVE KATD-TEXT-IDSKYLT TO MOD-IDSKYLT(RAD-IX 2)                  
039000       WHEN KATD-TEXT-IDSKYLT = 'USA'                                     
039100         MOVE KATD-TEXT-IDSKYLT TO MOD-IDSKYLT(RAD-IX 3)                  
039200       WHEN KATD-TEXT-IDSKYLT = 'D  '                                     
039300         MOVE KATD-TEXT-IDSKYLT TO MOD-IDSKYLT(RAD-IX 4)                  
039400       WHEN KATD-TEXT-IDSKYLT = 'F  '                                     
039500         MOVE KATD-TEXT-IDSKYLT TO MOD-IDSKYLT(RAD-IX 5)                  
039600       WHEN KATD-TEXT-IDSKYLT = 'E  '                                     
039700         MOVE KATD-TEXT-IDSKYLT TO MOD-IDSKYLT(RAD-IX 6)                  
039800       WHEN KATD-TEXT-IDSKYLT = 'P  '                                     
039900         MOVE KATD-TEXT-IDSKYLT TO MOD-IDSKYLT(RAD-IX 7)                  
040000       WHEN KATD-TEXT-IDSKYLT = 'NL '                                     
040100         MOVE KATD-TEXT-IDSKYLT TO MOD-IDSKYLT(RAD-IX 8)                  
040200       WHEN KATD-TEXT-IDSKYLT = 'I  '                                     
040300         MOVE KATD-TEXT-IDSKYLT TO MOD-IDSKYLT(RAD-IX 9)                  
040400       WHEN KATD-TEXT-IDSKYLT = 'SF '                                     
040500         MOVE KATD-TEXT-IDSKYLT TO MOD-IDSKYLT(RAD-IX 10)                 
040600       END-EVALUATE                                                       
040700     END-IF                                                               
040800     CONTINUE.                                                            
040900     EJECT                                                                
041000 D-KOLLA-PFTANGENTER SECTION.                                             
041100     SKIP2                                                                
041200     IF MFS-IDPFK = '7'                                                   
041300       MOVE ZERO TO WS-IDTTEXNR-SPAR                                      
041400     ELSE                                                                 
041500       EVALUATE TRUE                                                      
041600       WHEN MFS-IDPFK = '8'                                               
041700         IF MID-IDTTEXNR-SPAR NUMERIC                                     
041800           IF MID-IDTTEXNR-SPAR > ZERO                                    
041900             CONTINUE                                                     
042000           ELSE                                                           
042100             MOVE MED-2(INDX) TO MOD-TEMFSINF                             
042200           END-IF                                                         
042300           MOVE MID-IDTTEXNR-SPAR TO WS-IDTTEXNR-SPAR                     
042400         ELSE                                                             
042500           MOVE ZERO TO WS-IDTTEXNR-SPAR                                  
042600           MOVE MED-2(INDX) TO MOD-TEMFSINF                               
042700         END-IF                                                           
042800        WHEN OTHER                                                        
042900         INSPECT MID-IDTTEXNR REPLACING LEADING SPACE BY ZERO             
043000         IF MID-IDTTEXNR NUMERIC                                          
043100           MOVE MID-IDTTEXNR TO WS-IDTTEXNR-SPAR                          
043200         ELSE                                                             
043300           MOVE ZERO TO WS-IDTTEXNR-SPAR                                  
043400           MOVE MED-2(INDX) TO MOD-TEMFSINF                               
043500         END-IF                                                           
043600       END-EVALUATE                                                       
043700     END-IF                                                               
043800     CONTINUE.                                                            
043900     EJECT                                                                
044000* IMS SEKTIONER                                                           
044100     SKIP3                                                                
044200 IMS-GET-MSG SECTION.                                                     
044300     MOVE '  QC' TO GODK-STATUSKODER                                      
044400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
044500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
044600     PERFORM IMS-STATUSKONTROLL                                           
044700     CONTINUE.                                                            
044800     SKIP3                                                                
044900 IMS-INSERT-MSG SECTION.                                                  
045000     IF ENGLISH-TEXT                                                      
045100       MOVE 'N' TO MFS-KDHUVOMR                                           
045200     END-IF                                                               
045300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
045400     MOVE SPACE TO GODK-STATUSKODER                                       
045500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
045600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
045700     PERFORM IMS-STATUSKONTROLL                                           
045800     CONTINUE.                                                            
045900     EJECT                                                                
046000 IMS-GET-KATE01 SECTION.                                                  
046100     STRING 'WLKATE01(WDN4A1KY=>' W-IDSKYLT-X  W-IDTTEXNR-X               
046200                     '&WDN4A1KY <' W-IDSKYLT-X                            
046300                      W-IDTTEXNR-MAX-X ')'                                
046400            DELIMITED BY SIZE INTO SSA1                                   
046500     MOVE '  GBGE' TO GODK-STATUSKODER                                    
046600     CALL CBLTDLI USING GN KATE-PCB DLI-IO-AREA SSA1                      
046700     MOVE KATE-STATUS-CODE TO STATUS-WS                                   
046800     PERFORM IMS-STATUSKONTROLL                                           
046900     CONTINUE.                                                            
047000     SKIP3                                                                
047100 IMS-GET-KATD01 SECTION.                                                  
047200     STRING 'WLKATD01(IDTTEXNR =' W-IDTTEXNR-X ')'                        
047300            DELIMITED BY SIZE INTO SSA1                                   
047400     MOVE '  ' TO GODK-STATUSKODER                                        
047500     CALL CBLTDLI USING GU KATD-PCB DLI-IO-AREA SSA1                      
047600     MOVE KATD-STATUS-CODE TO STATUS-WS                                   
047700     PERFORM IMS-STATUSKONTROLL                                           
047800     CONTINUE.                                                            
047900     SKIP3                                                                
048000 IMS-GET-KATD11 SECTION.                                                  
048100     MOVE 'WLKATD11 ' TO SSA1                                             
048200     MOVE '  GE' TO GODK-STATUSKODER                                      
048300     CALL CBLTDLI USING GNP KATD-PCB DLI-IO-AREA SSA1                     
048400     MOVE KATD-STATUS-CODE TO STATUS-WS                                   
048500     PERFORM IMS-STATUSKONTROLL                                           
048600     CONTINUE.                                                            
048700     EJECT                                                                
048800 IMS-STATUSKONTROLL SECTION.                                              
048900     SET STATUS-IX TO 1                                                   
049000     SEARCH GODK-STATUS                                                   
049010       AT END                                                             
049020         CALL FELLOG                                                      
049100     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
049200     END-SEARCH                                                           
049400     CONTINUE.                                                            
