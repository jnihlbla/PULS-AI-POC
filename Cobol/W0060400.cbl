000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0060400.                                                
000300*AUTHOR.         RICHARD.                                                 
000400*DATE-WRITTEN.   MAJ  1984.                                               
000500                                                                          
000600*    FUNKTION.                                                            
000700*        PROGRAMMET HAR OLIKA INGÅNGAR ENL. NEDAN                         
000800                                                                          
000900*        1. FRÅGA                                                         
001000                                                                          
001100*        2. START OCH BORTTAG                                             
001200                                                                          
001300*           A. START GÖRS GENOM ATT                                       
001400*              TRYCKA PF11 (GÅR EJ OM DET FINNS BORTTAGS                  
001500*              .            MARKERADE FELARTIKLAR).                       
001600                                                                          
001700*           B. BORTTAG KAN GÖRAS FRÅN BILD 0604, GENOM ATT                
001800*              TRYCKA PF11 (MÅSTE VARA MÄRKTA MED ETT (B).                
001900                                                                          
002000*    INDATA.                                                              
002100*        TRANSAKTION: W0T604 (FRÅGA)                                      
002200*        MID:         W0I60401                                            
002300*        TRANSAKTION: W0T604U (START AV BMP OCH UPPDATERING)              
002400                                                                          
002500*    UTDATA.                                                              
002600*        TRANSAKTION: VBMP                                                
002700*        MOD:         W0O60401                                            
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000                                                                          
003100 DATA DIVISION.                                                           
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400                                                                          
003500*    -- CHECKED BY WY2000                                                 
003600 77    IDPGM                 PIC X(8)    VALUE 'W0060400'.                
003700 77    JA                    PIC X(1)    VALUE 'J'.                       
003800 77    NEJ                   PIC X(1)    VALUE 'N'.                       
003900 77    W-NYSIDA              PIC X(1)    VALUE '8'.                       
004000 77    W-KLAR                PIC S9(1)   VALUE +4    COMP-3.              
004100 77    W-BORT                PIC X(1)    VALUE 'B'.                       
004200 77    INDX                  PIC S9(9)   VALUE +0    COMP SYNC.           
004300 77    W-4802-START-IX       PIC S9(9)   VALUE +0    COMP SYNC.           
004400 77    W-4802-IX             PIC S9(9)   VALUE +0    COMP SYNC.           
004500 77    W-4804-IX             PIC S9(9)   VALUE +0    COMP SYNC.           
004600 77    MAX-4802-IX-PLUS-1    PIC S9(9)   VALUE +13   COMP SYNC.           
004700 77    MAX-4804-IX-PLUS-1    PIC S9(9)   VALUE +5    COMP SYNC.           
004800 77    MAX-MOD-LANGD         PIC S9(4)   VALUE +859  COMP SYNC.           
004900     SKIP3                                                                
005000                                                                          
005100 01    DYNAMISKA-SUBPROGRAM.                                              
005200   03    CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
005300   03    FELLOG              PIC X(8)    VALUE 'FELLOG  '.                
005400     SKIP3                                                                
005500                                                                          
005600 01    NYCKLAR-TILL-DLI.                                                  
005700   03    W-WDGX-4801-KEY-X.                                               
005800     05    FILLER            PIC X(4)    VALUE '4801'.                    
005900     05    FILLER            PIC X(26)   VALUE LOW-VALUE.                 
006000                                                                          
006100 01    W-PROG-TO-PROG-SW.                                                 
006200                                                                          
006300*  03  -COPY WMSGSOP                                                      
006400     EJECT                                                                
006500 01  FILLER                  PIC X(16)   VALUE 'TEXTER'.                  
006600                                                                          
006700 01    STATUS-TEXTER.                                                     
006800   03    FILLER              PIC X(20)   VALUE                            
006900                                         'ÖVERFÖRING BEGÄRD   '.          
007000   03    FILLER              PIC X(20)   VALUE                            
007100                                         'ÖVERFÖRING STARTAD  '.          
007200   03    FILLER              PIC X(20)   VALUE                            
007300                                         'ÖVERFÖRING AVBRUTEN '.          
007400   03    FILLER              PIC X(20)   VALUE                            
007500                                         'ÖVERFÖRING KLAR     '.          
007600   03    FILLER              PIC X(20)   VALUE                            
007700                                         'NÅGON FEL RING 2129 '.          
007800 01    FILLER REDEFINES STATUS-TEXTER.                                    
007900   03    STATUS-TEXT OCCURS 5 PIC X(20).                                  
008000     SKIP3                                                                
008100 01    FELTEXTER-4802.                                                    
008200   03    FILLER              PIC X(15)   VALUE                            
008300                                         'OKÄNT RING 2129'.               
008400   03    FILLER              PIC X(15)   VALUE                            
008500                                         'SALDO NEGATIVT '.               
008600   03    FILLER              PIC X(15)   VALUE                            
008700                                         'SAKNAS PÅ SALDO'.               
008800   03    FILLER              PIC X(15)   VALUE                            
008900                                         'PTYP EJ NUMER  '.               
009000   03    FILLER              PIC X(15)   VALUE                            
009100                                         'FELAKTIG PTYP  '.               
009200   03    FILLER              PIC X(15)   VALUE                            
009300                                         'ART.NR FEL     '.               
009400   03    FILLER              PIC X(15)   VALUE                            
009500                                         'ART.NR EJ NUMER'.               
009600   03    FILLER              PIC X(15)   VALUE                            
009700                                         'BUFF-F EJ NUMER'.               
009800   03    FILLER              PIC X(15)   VALUE                            
009900                                         'BUFF-O EJ NUMER'.               
010000 01    FILLER REDEFINES FELTEXTER-4802.                                   
010100   03    FELTEXT OCCURS 9    PIC X(15).                                   
010200     EJECT                                                                
010300 01    MEDDELANDE.                                                        
010400   03    MED-1               PIC X(40)   VALUE                            
010500                             'BMP STARTAD         '.                      
010600   03    MED-2               PIC X(40)   VALUE                            
010700                             'FÖREGÅENDE BMP EJ KLAR'.                    
010800   03    MED-3               PIC X(40)   VALUE                            
010900                             'FELPOSTER BORTTAGNA '.                      
011000     EJECT                                                                
011100******************************************************************        
011200*                                                                         
011300*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
011400*                                                                         
011500 01    FILLER                PIC X(16)   VALUE 'MFS-WS'.                  
011600     SKIP3                                                                
011700*01    MID -COPY W0I60401                                                 
011800     EJECT                                                                
011900*01    -COPY WMSGAREA                                                     
012000     EJECT                                                                
012100*  03    MOD -COPY W0O60401         -RED MSG-AREA.                        
012200     EJECT                                                                
012300*01    -COPY WMFSAREA                                                     
012400     EJECT                                                                
012500******************************************************************        
012600*                                                                         
012700*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012800*                                                                         
012900 01    IMS-WS.                                                            
013000   03    FILLER              PIC X(16)   VALUE 'IMS-WS     '.             
013100     SKIP3                                                                
013200*                        **** STATUS-KOD FRÅN IMS                         
013300   03    STATUS-WS           PIC XX.                                      
013400     88    SEGMENT-FINNS                 VALUE '  '.                      
013500     88    SEGMENT-SAKNAS                VALUE 'GE'.                      
013600     SKIP3                                                                
013700   03  GODK-STATUSKODER.                                                  
013800     05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.                
013900     SKIP3                                                                
014000 01    SSA1                  PIC X(64).                                   
014100 01    SSA2                  PIC X(64).                                   
014200     EJECT                                                                
014300*                            IMS FUNKTIONSKODER                           
014400*01    -COPY W0003                                                        
014500     EJECT                                                                
014600 01    FILLER                PIC X(16)   VALUE 'DLI-IOAREA'.              
014700                                                                          
014800*                            DLI INPUT-OUTPUT AREA                        
014900 01    DLI-IO-AREA.                                                       
015000   03    IO-AREA             PIC X(128)  VALUE SPACE.                     
015100     SKIP3                                                                
015200*  03    WLXXDD11 -COPY WDGX4802   -RED IO-AREA.                          
015300     EJECT                                                                
015400*  03    WLXXDD12 -COPY WDGX4804   -RED IO-AREA.                          
015500     EJECT                                                                
015600 LINKAGE SECTION.                                                         
015700*01    -COPY W0009     -PRE MSG-                                          
015800     SKIP2                                                                
015900*01    -COPY W0009     -PRE ALT-                                          
016000     SKIP2                                                                
016100*01    -COPY W0008     -PRE XXDD-                                         
016200     05  FILLER               PIC X.                                      
016300     EJECT                                                                
016400 PROCEDURE DIVISION USING MSG-PCB ALT-PCB XXDD-PCB.                       
016500     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB XXDD-PCB                       
016600                                                                          
016700     PERFORM IMS-GET-MSG                                                  
016800     IF SEGMENT-FINNS                                                     
016900       PERFORM A-INIT-SPARA-INPUT                                         
017000       PERFORM IMS-GET-4801                                               
017100       IF MFS-UPDATE                                                      
017200         IF MID-KDSVAR-TABELL = ALL '+' OR MFS-IDTRANS = '0507'           
017300           PERFORM C-STARTA-JOB-TILL-PDP                                  
017400         ELSE                                                             
017500           PERFORM D-TA-BORT-FELPOSTER                                    
017600           PERFORM IMS-GET-4804                                           
017700         END-IF                                                           
017800       ELSE                                                               
017900         IF MFS-IDPFK = W-NYSIDA                                          
018000           MOVE MID-KVSALDOPOST-FEL-TOM TO W-4802-START-IX                
018100         ELSE                                                             
018200           MOVE MID-KVSALDOPOST-FEL-FOM TO W-4802-START-IX                
018300         END-IF                                                           
018400         PERFORM B-LAES-BEHANDLA-4802                                     
018500         PERFORM IMS-GET-4804                                             
018600       END-IF                                                             
018700       PERFORM E-FLYTTA-4804-TILL-MOD                                     
018800       MOVE MAX-MOD-LANGD TO MSG-KVLL                                     
018900       PERFORM IMS-INSERT-MSG                                             
019000     END-IF                                                               
019100     MOVE ZERO TO RETURN-CODE                                             
019200     GOBACK                                                               
019300     CONTINUE.                                                            
019400     EJECT                                                                
019500 A-INIT-SPARA-INPUT SECTION.                                              
019600                                                                          
019700     IF MSG-DUBBLA-TRANSKODER                                             
019800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I60401                 
019900       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
020000       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
020100       MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                  
020200       MOVE MSG-IDPFK                     TO MFS-IDPFK                    
020300     ELSE                                                                 
020400       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W0I60401                 
020500       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
020600       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
020610       MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                  
020700     END-IF                                                               
020800     IF MFS-IDTRANS = '0604' OR '0507'                                    
020900       IF MID-KVSALDOPOST-FEL-FOM    NOT NUMERIC                          
021000          OR MID-KVSALDOPOST-FEL-TOM NOT NUMERIC                          
021100          OR MFS-IDPFK = '7'                                              
021200         MOVE ZERO TO MID-KVSALDOPOST-FEL-FOM                             
021300                      MID-KVSALDOPOST-FEL-TOM                             
021400       END-IF                                                             
021500     ELSE                                                                 
021600       MOVE SPACE TO MFS-KDTRTYP                                          
021700                     MFS-IDPFK                                            
021800       MOVE ZERO TO MID-KVSALDOPOST-FEL-FOM                               
021900                    MID-KVSALDOPOST-FEL-TOM                               
022000     END-IF                                                               
022100     MOVE LOW-VALUE TO MSG-AREA                                           
022200     MOVE 'W0O60401' TO MFS-IDMOD                                         
022300     MOVE '0604' TO MOD-IDTRANS                                           
022400                                                                          
022500     MOVE '0604'       TO MSGSOP-IDTRANS                                  
022600     MOVE MFS-KDMFSFOR TO MSGSOP-KDMFSFOR                                 
022700                                                                          
022800     MOVE MFS-RENSA-FAELT TO MOD-MESSAGE-RAD1                             
022900                             MOD-MESSAGE-RAD23                            
023100     CONTINUE.                                                            
023200     EJECT                                                                
023300 B-LAES-BEHANDLA-4802 SECTION.                                            
023400                                                                          
023500     MOVE +1 TO INDX                                                      
023600     MOVE +1 TO W-4802-IX                                                 
023700     PERFORM IMS-GET-4802                                                 
023800     PERFORM UNTIL                                                        
023900      NOT ( W-4802-IX < MAX-4802-IX-PLUS-1 )                              
024000       IF SEGMENT-FINNS                                                   
024100         IF INDX < W-4802-START-IX                                        
024200           CONTINUE                                                       
024300         ELSE                                                             
024400           PERFORM BA-FLYTTA-4802                                         
024500           ADD +1 TO W-4802-IX                                            
024600         END-IF                                                           
024700         ADD +1 TO INDX                                                   
024800         PERFORM IMS-GET-4802                                             
024900       ELSE                                                               
025000         PERFORM MFS-RENSA-FAELT-4802                                     
025100         ADD +1 TO W-4802-IX                                              
025200       END-IF                                                             
025300     END-PERFORM                                                          
025400     MOVE W-4802-START-IX TO MOD-KVSALDOPOST-FEL-FOM                      
025500     IF SEGMENT-FINNS                                                     
025600       MOVE INDX TO MOD-KVSALDOPOST-FEL-TOM                               
025700     ELSE                                                                 
025800       MOVE ZERO TO MOD-KVSALDOPOST-FEL-TOM                               
025900     END-IF                                                               
026000     CONTINUE.                                                            
026100     EJECT                                                                
026200 BA-FLYTTA-4802 SECTION.                                                  
026300                                                                          
026400     MOVE MFS-OEPPNA-ALFA-FAELT TO MOD-KDSVAR-ATTR (W-4802-IX)            
026500     MOVE MFS-RENSA-FAELT TO MOD-KDSVAR (W-4802-IX)                       
026600     MOVE 4802-IDLOPNRF TO MOD-IDLOPNRF (W-4802-IX)                       
026700     MOVE 4802-IDARTNR  TO MOD-IDARTNR  (W-4802-IX)                       
026800     MOVE 4802-TIREGDAT TO MOD-TIREGDAT (W-4802-IX)                       
026900                                                                          
027000     EVALUATE 4802-IDFELKOD                                               
027100     WHEN '221'                                                           
027200       MOVE FELTEXT (2) TO MOD-FELTEXT (W-4802-IX)                        
027300     WHEN '222'                                                           
027400       MOVE FELTEXT (3) TO MOD-FELTEXT (W-4802-IX)                        
027500     WHEN '201'                                                           
027600       MOVE FELTEXT (4) TO MOD-FELTEXT (W-4802-IX)                        
027700     WHEN '202'                                                           
027800       MOVE FELTEXT (5) TO MOD-FELTEXT (W-4802-IX)                        
027900     WHEN '203'                                                           
028000       MOVE FELTEXT (6) TO MOD-FELTEXT (W-4802-IX)                        
028100     WHEN '204'                                                           
028200       MOVE FELTEXT (7) TO MOD-FELTEXT (W-4802-IX)                        
028300     WHEN '205'                                                           
028400       MOVE FELTEXT (8) TO MOD-FELTEXT (W-4802-IX)                        
028500     WHEN '206'                                                           
028600       MOVE FELTEXT (9) TO MOD-FELTEXT (W-4802-IX)                        
028700     WHEN OTHER                                                           
028800       MOVE FELTEXT (1) TO MOD-FELTEXT (W-4802-IX)                        
028900     END-EVALUATE                                                         
029000     CONTINUE.                                                            
029100     EJECT                                                                
029200 C-STARTA-JOB-TILL-PDP SECTION.                                           
029300     PERFORM MFS-ROER-EJ-FAELT-4802                                       
029400     PERFORM IMS-GET-4804                                                 
029500     IF 4804-KDTRSTAT (1) = W-KLAR                                        
029600       MOVE 4804-RAD (3) TO 4804-RAD (4)                                  
029700       MOVE 4804-RAD (2) TO 4804-RAD (3)                                  
029800       MOVE 4804-RAD (1) TO 4804-RAD (2)                                  
029900                                                                          
030000       ACCEPT 4804-TIREGDAT-START (1) FROM DATE                           
030100       ACCEPT 4804-TIUPPTID-START (1) FROM TIME                           
030200       MOVE ZERO TO 4804-TIUPPTID-KLAR   (1)                              
030300                    4804-KVSALDOPOST-PDP (1)                              
030400                    4804-KVSALDOPOST-IBM (1)                              
030500                    4804-KVSALDOPOST-FEL (1)                              
030600       MOVE +1   TO 4804-KDTRSTAT        (1)                              
030700                                                                          
030800       PERFORM IMS-REPLACE-4804                                           
030900                                                                          
031000       MOVE 'W488J018'   TO MSGSOP-IDPROCESS                              
031100       PERFORM IMS-INSERT-ALT                                             
031200                                                                          
031300       MOVE MED-1 TO MOD-MESSAGE-RAD23                                    
031400     ELSE                                                                 
031500       MOVE MED-2 TO MOD-MESSAGE-RAD1                                     
031600     END-IF                                                               
031700     CONTINUE.                                                            
031800     EJECT                                                                
031900 D-TA-BORT-FELPOSTER SECTION.                                             
032000                                                                          
032100     PERFORM MFS-ROER-EJ-FAELT-4802                                       
032200     MOVE MID-KVSALDOPOST-FEL-FOM TO W-4802-START-IX                      
032300     MOVE +1 TO INDX                                                      
032400     MOVE +1 TO W-4802-IX                                                 
032500     PERFORM IMS-GET-4802                                                 
032600     PERFORM UNTIL                                                        
032700      NOT ( W-4802-IX < MAX-4802-IX-PLUS-1 )                              
032800       IF SEGMENT-FINNS                                                   
032900         IF INDX < W-4802-START-IX                                        
033000           CONTINUE                                                       
033100         ELSE                                                             
033200           IF MID-KDSVAR (W-4802-IX) = W-BORT                             
033300             PERFORM IMS-DELETE-4802                                      
033400             PERFORM MFS-RENSA-FAELT-4802                                 
033500             MOVE MED-3 TO MOD-MESSAGE-RAD23                              
033600           END-IF                                                         
033700           ADD +1 TO W-4802-IX                                            
033800         END-IF                                                           
033900         ADD +1 TO INDX                                                   
034000         PERFORM IMS-GET-4802                                             
034100       ELSE                                                               
034200         MOVE MAX-4802-IX-PLUS-1 TO W-4802-IX                             
034300       END-IF                                                             
034400     END-PERFORM                                                          
034500     CONTINUE.                                                            
034600     EJECT                                                                
034700 E-FLYTTA-4804-TILL-MOD SECTION.                                          
034800                                                                          
034900     MOVE +1 TO W-4804-IX                                                 
035000                                                                          
035100     PERFORM UNTIL                                                        
035200      NOT ( W-4804-IX < MAX-4804-IX-PLUS-1 )                              
035300       MOVE 4804-TIREGDAT-START  (W-4804-IX)                              
035400                                TO MOD-TIREGDAT-START                     
035500       (W-4804-IX)                                                        
035600       MOVE 4804-TIUPPTID-START  (W-4804-IX)                              
035700                                TO MOD-TIUPPTID-START                     
035800       (W-4804-IX)                                                        
035900       MOVE 4804-TIUPPTID-KLAR   (W-4804-IX)                              
036000                                TO MOD-TIUPPTID-KLAR                      
036100       (W-4804-IX)                                                        
036200       MOVE 4804-KVSALDOPOST-PDP (W-4804-IX)                              
036300                                TO MOD-KVSALDOPOST-PDP                    
036400       (W-4804-IX)                                                        
036500       MOVE 4804-KVSALDOPOST-IBM (W-4804-IX)                              
036600                                TO MOD-KVSALDOPOST-IBM                    
036700       (W-4804-IX)                                                        
036800       MOVE 4804-KVSALDOPOST-FEL (W-4804-IX)                              
036900                                TO MOD-KVSALDOPOST-FEL                    
037000       (W-4804-IX)                                                        
037100                                                                          
037200       IF 4804-KDTRSTAT (W-4804-IX) > +0 AND < +5                         
037300         MOVE 4804-KDTRSTAT (W-4804-IX) TO INDX                           
037400       ELSE                                                               
037500         MOVE +5 TO INDX                                                  
037600       END-IF                                                             
037700       MOVE STATUS-TEXT (INDX) TO MOD-STATUS-TEXT (W-4804-IX)             
037800       ADD +1 TO W-4804-IX                                                
037900     END-PERFORM                                                          
038000     CONTINUE.                                                            
038100     EJECT                                                                
038200 MFS-ROER-EJ-FAELT-4802 SECTION.                                          
038300                                                                          
038400     MOVE MFS-ROER-EJ-FAELT TO MOD-KVSALDOPOST-FEL-FOM                    
038500                               MOD-KVSALDOPOST-FEL-TOM                    
038600     MOVE +1 TO W-4802-IX                                                 
038700     PERFORM UNTIL                                                        
038800      NOT ( W-4802-IX < MAX-4802-IX-PLUS-1 )                              
038900       MOVE MFS-ROER-EJ-FAELT TO MOD-KDSVAR   (W-4802-IX)                 
039000                                 MOD-IDLOPNRF (W-4802-IX)                 
039100       ADD +1 TO W-4802-IX                                                
039200     END-PERFORM                                                          
039300     CONTINUE.                                                            
039400     SKIP3                                                                
039500 MFS-RENSA-FAELT-4802 SECTION.                                            
039600                                                                          
039700     MOVE MFS-RENSA-FAELT TO MOD-KDSVAR   (W-4802-IX)                     
039800                             MOD-IDLOPNRF (W-4802-IX)                     
039900     .                                                                    
040000     EJECT                                                                
040100* IMS SEKTIONER                                                           
040200     SKIP3                                                                
040300 IMS-GET-MSG SECTION.                                                     
040400     MOVE '  QC' TO GODK-STATUSKODER                                      
040500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
040600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
040700     PERFORM IMS-STATUSKONTROLL                                           
040800     .                                                                    
040900     SKIP3                                                                
041000 IMS-INSERT-MSG SECTION.                                                  
041100     IF ENGLISH-TEXT                                                      
041200       MOVE 'R' TO MFS-KDHUVOMR                                           
041300     END-IF                                                               
041400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
041500     MOVE SPACE TO GODK-STATUSKODER                                       
041600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
041700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
041800     PERFORM IMS-STATUSKONTROLL                                           
041900     .                                                                    
042000     SKIP3                                                                
042100 IMS-INSERT-ALT SECTION.                                                  
042200     MOVE SPACE TO GODK-STATUSKODER                                       
042300     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
042400     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
042500     PERFORM IMS-STATUSKONTROLL                                           
042600     .                                                                    
042700     EJECT                                                                
042800 IMS-GET-4801 SECTION.                                                    
042900     STRING 'WLXXDD01(WDGXKEY  =' W-WDGX-4801-KEY-X ') '                  
043000            DELIMITED BY SIZE INTO SSA1                                   
043100     MOVE '  ' TO GODK-STATUSKODER                                        
043200     CALL CBLTDLI USING GU XXDD-PCB DLI-IO-AREA SSA1                      
043300     MOVE XXDD-STATUS-CODE TO STATUS-WS                                   
043400     PERFORM IMS-STATUSKONTROLL                                           
043500     .                                                                    
043600     SKIP3                                                                
043700 IMS-GET-4802 SECTION.                                                    
043800     MOVE 'WLXXDD11 ' TO SSA1                                             
043900     MOVE '  GE' TO GODK-STATUSKODER                                      
044000     CALL CBLTDLI USING GHNP XXDD-PCB DLI-IO-AREA SSA1                    
044100     MOVE XXDD-STATUS-CODE TO STATUS-WS                                   
044200     PERFORM IMS-STATUSKONTROLL                                           
044300     .                                                                    
044400     SKIP3                                                                
044500 IMS-GET-4804 SECTION.                                                    
044600     MOVE 'WLXXDD12 ' TO SSA1                                             
044700     MOVE '  ' TO GODK-STATUSKODER                                        
044800     CALL CBLTDLI USING GHNP XXDD-PCB DLI-IO-AREA SSA1                    
044900     MOVE XXDD-STATUS-CODE TO STATUS-WS                                   
045000     PERFORM IMS-STATUSKONTROLL                                           
045100     .                                                                    
045200     EJECT                                                                
045300 IMS-DELETE-4802 SECTION.                                                 
045400     MOVE '  ' TO GODK-STATUSKODER                                        
045500     CALL CBLTDLI USING DLET XXDD-PCB DLI-IO-AREA                         
045600     MOVE XXDD-STATUS-CODE TO STATUS-WS                                   
045700     PERFORM IMS-STATUSKONTROLL                                           
045800     .                                                                    
045900     SKIP3                                                                
046000 IMS-REPLACE-4804 SECTION.                                                
046100     MOVE '  ' TO GODK-STATUSKODER                                        
046200     CALL CBLTDLI USING REPL XXDD-PCB DLI-IO-AREA                         
046300     MOVE XXDD-STATUS-CODE TO STATUS-WS                                   
046400     PERFORM IMS-STATUSKONTROLL                                           
046500     .                                                                    
046600     EJECT                                                                
046700 IMS-STATUSKONTROLL SECTION.                                              
046800     SET STATUS-IX TO 1                                                   
046900     SEARCH GODK-STATUS                                                   
047000       AT END                                                             
047100         CALL FELLOG                                                      
047200     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
047300       CONTINUE                                                           
047400     END-SEARCH                                                           
047500     .                                                                    
