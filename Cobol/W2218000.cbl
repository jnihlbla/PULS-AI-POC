000100 ID DIVISION.                                                             
000200*                                                                         
000300 PROGRAM-ID.             W2218000.                                        
000400 AUTHOR.                 KENT HELLQVIST.                                  
000500 DATE-WRITTEN.           DECEMBER 1985.                                   
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    ANN J HAR ÄNDRAT I DETTA PGM FÖR DELNINGEN. KLAR 901204              
001000*    (GJORT COBMETA)                                                      
001100*                                                                         
001200*    NY COPYTEXT PÅ RLBKXX11.                    KLAR 910322              
001300*    (GJORT COBCONV)                                                      
001400*                                                                         
001500*    FUNKTION: PROGRAMMET LÄSER EN LEVERANTÖR PÅ WLXXBK. MED DENNA        
001600*              SOM NYCKEL HÄMTAS SAMTLIGA ARTIKLAR MED DENNA LEV-         
001700*              RANTÖR PÅ WLXXBL.FÖR VARJE ARTIKEL SKAPAS EN UTPOST        
001800*              OCH SEGMENTET WLXXBL11 DELETAS.                            
001900*              LEVERANTÖRENS TISEND-SEN OCH IDOVERFNR UPPDATERAS          
002000*              OCH REPLACE SKER AV SEGMENTET.(WLXXBK11).                  
002100*                                                                         
002200*    NYTT APRIL: TVÅ UTFILER SKAPAS, EN FÖR LEVERANTÖRER MED ÖVER-        
002300*    -90         FÖRING VIA VDA (DET GAMLA) OCH EN FÖR LEVERANTÖRE        
002400*                MED ÖVERFÖRING VIA ODETTE.                               
002500*    ---         BORTTAGEN 20190415 - FINNS INGA VDA KVAR.                
002600*    ---         UT-FIL W22180 TAS EJ UPP NÅGONSTANS, TOM-FIL.            
002700*                                                                         
002800*    NYTT HT-93: NY FIL(NÄR PGM BLIR BMP) MED LEVNR + ARTNR               
002900*                FÖR ATT I W22182 GÖRA DLET PÅ WLXXBL11                   
003000*    SUBPROGRAM:                                                          
003100*            W2218010   - SKÖTER ALLA ANROP MOT IMS                       
003200*                                                                         
003300     EJECT                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500                                                                          
003600 INPUT-OUTPUT SECTION.                                                    
003700                                                                          
003800 FILE-CONTROL.                                                            
003900                                                                          
004000*- - - - - - - - - - - - - - UTFILER:                                     
004100                                                                          
004200     SELECT  W22181                   ASSIGN TO W22180D1.                 
004300     SELECT  W22182                   ASSIGN TO W22180D2.                 
004400     EJECT                                                                
004500 DATA DIVISION.                                                           
004600     SKIP2                                                                
004700 FILE SECTION.                                                            
004800     SKIP3                                                                
004900                                                                          
005000 FD  W22181                                                               
005100     LABEL RECORD STANDARD                                                
005200     RECORDING F                                                          
005300     BLOCK CONTAINS 0.                                                    
005400     SKIP2                                                                
005500*01  UTPOST2 -COPY W22181      -L.                                        
005600     EJECT                                                                
005700 FD  W22182                                                               
005800     LABEL RECORD STANDARD                                                
005900     RECORDING F                                                          
006000     BLOCK CONTAINS 0.                                                    
006100     SKIP2                                                                
006200*01  UTPOST3 -COPY W22182      -L.                                        
006300     EJECT                                                                
006400 WORKING-STORAGE SECTION.                                                 
006500     SKIP2                                                                
006600*    -COPY WY2000W1                                                       
006700     SKIP3                                                                
006800*- - - - - - - - - - - - - -  GENERERAT PROGRAM-NAMN                      
006900 77   PROGRAM-NAMN           VALUE 'W2218000'                             
007000                                 PIC X(8).                                
007100     SKIP2                                                                
007200*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
007300                                                                          
007400 77  JA                          PIC X       VALUE 'J'.                   
007500 77  NEJ                         PIC X       VALUE 'N'.                   
007510 77  SW-SKRIV-UT-W22181          PIC X       VALUE 'J'.                   
007600                                                                          
007700 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
007800                                                                          
007900 01  SPAR-FALT.                                                           
008000     05 SPAR-TISEND-SEN-AAR-VV.                                           
008100        10  SPAR-TISEND-SEN-AAR  PIC 9(02).                               
008200        10  SPAR-TISEND-SEN-VV   PIC 9(02).                               
008300                                                                          
008400 01  ARBETS-FALT.                                                         
008500   03  WS-IDLPLAN                PIC 9(7)    VALUE ZERO.                  
008600   03  WS-IDAAVVLLL.                                                      
008700     05 WS-IDAAVV                PIC 9(4).                                
008800     05 WS-LLL                   PIC 9(3).                                
008900     EJECT                                                                
009000 01  DYNAMISKA-SUBPROGRAM.                                                
009100   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
009200   03  W2218010                  PIC X(8)    VALUE 'W2218010'.            
009300   03  DATKORT                   PIC X(8)    VALUE 'DATKORT '.            
009400   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
009500     SKIP3                                                                
009600 01  WS-DAGENS-DATUM.                                                     
009700     03  DAGENS-AA               PIC 9(02).                               
009800     03  DAGENS-MM               PIC 9(02).                               
009900     03  DAGENS-DD               PIC 9(02).                               
010000                                                                          
010100 01  DAGENS-DATUM                PIC 9(06).                               
010200                                                                          
010210 01  DAGENS-DAGNR                PIC 9(1) VALUE ZERO.                     
010220                                                                          
010300 01  DAGENS-AAR-VV.                                                       
010400     05  DAGENS-AAR              PIC 9(02).                               
010500     05  DAGENS-VV               PIC 9(02).                               
010600     EJECT                                                                
010700*01  WS           -COPY W22181       -PRE UTPOST2-.                       
010800     EJECT                                                                
010900*01  WS           -COPY W22182       -PRE UTPOST3-.                       
011000     EJECT                                                                
011100*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
011200                                                                          
011300*01  -COPY W0005       -PRE POSTSUM-.                                     
011400     EJECT                                                                
011500*- - - - - - - - - - - - - -  PARAMETRAR TILL DATKORT                     
011600                                                                          
011700*01  -COPY WDATKORTC0  -PRE DATKORT-.                                     
011800     EJECT                                                                
011900*- - - - - - - - - - - - - -  PARAMETRAR TILL WDATKONV                    
012000                                                                          
012100*01  -COPY WDATAREA.                                                      
012200     EJECT                                                                
012300*- - - - - - - - - - - - - - PARAMETER-AREOR TILL IMS-SUBPGM              
012400*                                                                         
012500 01  FILLER                      PIC X(24)    VALUE                       
012600                                              '0-AREA-START'.             
012700                                                                          
012800*01  0-AREA    -COPY W221L800 -PRE CALL                                   
012900     EJECT                                                                
013000 01  FILLER                      PIC X(24)    VALUE                       
013100                                              '1-AREA-START'.             
013200                                                                          
013300*01  1-AREA    -COPY W221L801 -PRE CALL                                   
013400     EJECT                                                                
013500 01  FILLER                      PIC X(24)    VALUE                       
013600                                              '2-AREA-START'.             
013700                                                                          
013800*01  2-AREA    -COPY W221L802 -PRE CALL                                   
013900     EJECT                                                                
014000 01  FILLER                      PIC X(24)    VALUE                       
014100                                              '3-AREA-START'.             
014200                                                                          
014300*01  3-AREA    -COPY W221L803 -PRE CALL                                   
014400     EJECT                                                                
014500 LINKAGE SECTION.                                                         
014600     SKIP3                                                                
014700*01  -COPY  W0009  -PRE MSG-                                              
014800     SKIP3                                                                
014900 01  WLXXBL-PCB              PIC X(36).                                   
015000 01  WLXXBK-PCB              PIC X(36).                                   
015100 01  WLARTC-PCB              PIC X(36).                                   
015200     EJECT                                                                
015300 PROCEDURE DIVISION USING  MSG-PCB                                        
015400                        WLXXBL-PCB WLXXBK-PCB WLARTC-PCB.                 
015500     ENTRY 'DLITCBL' USING MSG-PCB                                        
015600                        WLXXBL-PCB WLXXBK-PCB WLARTC-PCB.                 
015700                                                                          
015800 STYR SECTION.                                                            
015900     SKIP3                                                                
016000     PERFORM A-INIT                                                       
016100                                                                          
016200     PERFORM B-LAES-WLXXBK01                                              
016300     PERFORM C-LAES-WLXXBK11                                              
016400                                                                          
016500     PERFORM UNTIL CALL0-KDSVAR NOT = CALL0-KDSVAR-OK                     
016600*       TAG REDA PÅ SENASTE SÄNDNINGSVECKA TILL LEVERANTÖREN              
016700        MOVE 'AAMMDD'             TO DAT-KDDATFORM                        
016800        MOVE CALL1-TISEND-SEN     TO DAT-I-TIDATUM                        
016900        CALL WDATKONV USING DAT-KDDATFORM                                 
017000                            DAT-I-TIDATUM                                 
017100                            DAT-O-TIDATUM                                 
017200                            DAT-KDSVAR                                    
017300        MOVE DAT-TIAA             TO SPAR-TISEND-SEN-AAR                  
017400        MOVE DAT-TIVV             TO SPAR-TISEND-SEN-VV                   
017600                                                                          
017700        IF  (DATKORT-D-DAGNR         = +3                AND              
017800             CALL1-KDVECKOSL         = 'V')                               
017900*           OM DET ÄR ONSDAG OCH LEVERANTÖREN HAR FAST ONSDAGS-           
018000*           SÄNDNING (KDVECKOSL).                                         
018010*           ÄR DET VECKOBATCH-TORSDAG SÅ SKALL EJ DAGFILEN SKAPAS.        
018011                                                                          
018030            IF  CALL1-FLLEVVB        = 'J'  AND                           
018040                DAGENS-DAGNR         = 4                                  
018050                                                                          
018060               MOVE NEJ TO SW-SKRIV-UT-W22181                             
018070            ELSE                                                          
018071               MOVE JA  TO SW-SKRIV-UT-W22181                             
018072            END-IF                                                        
018080            PERFORM D-LAES-WLXXBL-SKRIV-UTPOST                            
018110        ELSE                                                              
018200          IF CALL1-KDVECKOSL          = 'D'                               
018300*           LEVERANTÖREN HAR FAST DAGLIG SÄNDNING (KDVECKOSL)             
018310*           ÄR DET VECKOBATCH-MÅNDAG SÅ SKALL EJ DAGFILEN SKAPAS.         
018400                                                                          
018501            IF CALL1-FLLEVVB        = 'J'  AND                            
018511               DAGENS-DAGNR         = 1                                   
018520                                                                          
018530               MOVE NEJ TO SW-SKRIV-UT-W22181                             
018540            ELSE                                                          
018541               MOVE JA  TO SW-SKRIV-UT-W22181                             
018550            END-IF                                                        
018600            PERFORM D-LAES-WLXXBL-SKRIV-UTPOST                            
018701          END-IF                                                          
018710        END-IF                                                            
018820                                                                          
018900        PERFORM C-LAES-WLXXBK11                                           
019000     END-PERFORM                                                          
019100                                                                          
019200     PERFORM Z-FINIT                                                      
019300     MOVE ZERO TO RETURN-CODE                                             
019400     GOBACK                                                               
019500     .                                                                    
019600     EJECT                                                                
019700 A-INIT SECTION.                                                          
019800     SKIP3                                                                
019900     OPEN OUTPUT W22181                                                   
020000                 W22182                                                   
020100                                                                          
020200     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
020300                                                                          
020400     ACCEPT DAGENS-DATUM FROM DATE                                        
020401                                                                          
020410*--  TAG REDA PÅ DAGENS DAGNUMMER, EJ FRÅN DATKORTET.                     
020411*--  W221D2 KÖRS PÅ MÅNDAG MED FREDAGENS DATUM I DATKORT.                 
020412*--  W221D2 KÖRS PÅ TORSDAG MED ONSDAGENS DATUM I DATKORT.                
020413                                                                          
020414     MOVE 'IDAG  '    TO DAT-KDDATFORM                                    
020440     CALL WDATKONV USING DAT-KDDATFORM                                    
020450                         DAT-I-TIDATUM                                    
020460                         DAT-O-TIDATUM                                    
020470                         DAT-KDSVAR                                       
020471                                                                          
020480     MOVE DAT-TID     TO DAGENS-DAGNR                                     
020490                                                                          
020491                                                                          
020500*--- HÄMTA DAGENS-DATUM FRÅN DATKORT                                      
020510                                                                          
020600     CALL DATKORT USING PROGRAM-NAMN                                      
020700                        DATUMKORT-ID                                      
020800                        DATKORT-DATUMKORT                                 
020900                                                                          
021000     MOVE DATKORT-D-AAR       TO DAGENS-AAR                               
021100                                 DAGENS-AA                                
021200     MOVE DATKORT-D-VECKA     TO DAGENS-VV                                
021300     MOVE DATKORT-D-MAANAD    TO DAGENS-MM                                
021400     MOVE DATKORT-D-DAG       TO DAGENS-DD                                
021500     MOVE WS-DAGENS-DATUM     TO DAGENS-DATUM                             
021510                                                                          
021520     MOVE JA  TO SW-SKRIV-UT-W22181                                       
021600     .                                                                    
021700     EJECT                                                                
021800 B-LAES-WLXXBK01 SECTION.                                                 
021900     SKIP3                                                                
022000     MOVE CALL0-LAES-WLXXBK01 TO CALL0-KDCALL                             
022100                                                                          
022200     CALL W2218010 USING CALL0-AREA                                       
022300                         CALL1-AREA                                       
022400                         WLXXBL-PCB                                       
022500                         WLXXBK-PCB                                       
022600                         WLARTC-PCB                                       
022700     .                                                                    
022800     EJECT                                                                
022900 C-LAES-WLXXBK11 SECTION.                                                 
023000     SKIP3                                                                
023100     MOVE CALL0-LAES-WLXXBK11 TO CALL0-KDCALL                             
023200                                                                          
023300     CALL W2218010 USING CALL0-AREA                                       
023400                         CALL1-AREA                                       
023500                         WLXXBL-PCB                                       
023600                         WLXXBK-PCB                                       
023700                         WLARTC-PCB                                       
023800     .                                                                    
023900     EJECT                                                                
024000 D-LAES-WLXXBL-SKRIV-UTPOST SECTION.                                      
024100     SKIP3                                                                
024200     PERFORM DA-LAES-WLXXBL01                                             
024300     MOVE CALL1-IDLEVNR          TO CALL0-IDLEVNR                         
024400                                                                          
024500     PERFORM DB-LAES-WLXXBL11-KVAL                                        
024600                                                                          
024700     IF CALL0-KDSVAR = CALL0-KDSVAR-OK                                    
024800        PERFORM UNTIL CALL0-KDSVAR NOT = CALL0-KDSVAR-OK                  
024830                                                                          
024900           IF CALL1-KDEDI = 'O' OR 'F' OR 'E'                             
025000              MOVE CALL2-IDARTNR TO UTPOST2-IDARTNR                       
025100              MOVE CALL2-IDLEVNR TO UTPOST2-IDLEVNR                       
025200              MOVE CALL2-IDARTNR TO CALL0-IDARTNR                         
025300              PERFORM DC-LAES-WLARTC01                                    
025400              IF CALL0-KDSVAR = CALL0-KDSVAR-OK                           
025500                 MOVE CALL3-IDFTG TO UTPOST2-IDFTG                        
025600                 MOVE CALL3-KDPRODSL TO UTPOST2-KDPRODSL                  
025700              ELSE                                                        
025800                 MOVE ZERO TO UTPOST2-IDFTG                               
025900                              UTPOST2-KDPRODSL                            
026000              END-IF                                                      
026100                                                                          
026110              IF SW-SKRIV-UT-W22181 = JA                                  
026200                PERFORM DD-SKRIV-W22181-UTPOST                            
026300              END-IF                                                      
026310           END-IF                                                         
026400***        KDEDI = O , V , T  , F  ????                                   
026500           PERFORM DE-DELETE-WLXXBL11                                     
026600           PERFORM DB-LAES-WLXXBL11-KVAL                                  
026700        END-PERFORM                                                       
026800                                                                          
026900        ADD +1 TO CALL1-IDOVERFNR                                         
027000        PERFORM DF-REPL-WLXXBK11                                          
027100     END-IF                                                               
027200                                                                          
027300     .                                                                    
027400     EJECT                                                                
027500 DA-LAES-WLXXBL01 SECTION.                                                
027600     SKIP3                                                                
027700     MOVE CALL0-LAES-WLXXBL01 TO CALL0-KDCALL                             
027800                                                                          
027900     CALL W2218010 USING CALL0-AREA                                       
028000                         CALL2-AREA                                       
028100                         WLXXBL-PCB                                       
028200                         WLXXBK-PCB                                       
028300                         WLARTC-PCB                                       
028400     .                                                                    
028500     EJECT                                                                
028600 DB-LAES-WLXXBL11-KVAL SECTION.                                           
028700     SKIP3                                                                
028800     MOVE CALL0-LAES-WLXXBL11-KVAL TO CALL0-KDCALL                        
028900                                                                          
029000     CALL W2218010 USING CALL0-AREA                                       
029100                         CALL2-AREA                                       
029200                         WLXXBL-PCB                                       
029300                         WLXXBK-PCB                                       
029400                         WLARTC-PCB                                       
029500     .                                                                    
029600     EJECT                                                                
029700 DE-DELETE-WLXXBL11 SECTION.                                              
029800     SKIP3                                                                
029900     MOVE CALL2-IDLEVNR  TO UTPOST3-IDLEVNR                               
030000     MOVE CALL2-IDARTNR  TO UTPOST3-IDARTNR                               
030100                                                                          
030200     PERFORM DEA-SKRIV-W22182-UTPOST                                      
030300                                                                          
030400*    DELETE SKER AV KÖRNINGSTEKNISKA SKÄL I EFTERFÖLJANDE                 
030500*    PROGRAM W22182                                                       
030600     .                                                                    
030700     SKIP3                                                                
030800 DEA-SKRIV-W22182-UTPOST SECTION.                                         
030900     SKIP3                                                                
031000     WRITE UTPOST3 FROM UTPOST3-WS                                        
031100                                                                          
031200     MOVE 'W22182'   TO POSTSUM-FDNAMN                                    
031300     MOVE 'W22180D2' TO POSTSUM-DDNAMN2                                   
031400     CALL POSTSUM USING POSTSUM-PARM                                      
031500     .                                                                    
031600     EJECT                                                                
031700 DF-REPL-WLXXBK11 SECTION.                                                
031800     SKIP3                                                                
031900     MOVE CALL0-REPL-WLXXBK11 TO CALL0-KDCALL                             
032000                                                                          
032100     CALL W2218010 USING CALL0-AREA                                       
032200                         CALL1-AREA                                       
032300                         WLXXBL-PCB                                       
032400                         WLXXBK-PCB                                       
032500                         WLARTC-PCB                                       
032600                                                                          
032700     MOVE 'REPL  '   TO POSTSUM-FDNAMN                                    
032800     MOVE 'XXBK11  ' TO POSTSUM-DDNAMN2                                   
032900     CALL POSTSUM USING POSTSUM-PARM                                      
033000     .                                                                    
033100     EJECT                                                                
033200 DC-LAES-WLARTC01 SECTION.                                                
033300     SKIP3                                                                
033400     MOVE CALL0-LAES-WLARTC01 TO CALL0-KDCALL                             
033500                                                                          
033600     CALL W2218010 USING CALL0-AREA                                       
033700                         CALL3-AREA                                       
033800                         WLXXBL-PCB                                       
033900                         WLXXBK-PCB                                       
034000                         WLARTC-PCB                                       
034100     .                                                                    
034200     EJECT                                                                
034300 DD-SKRIV-W22181-UTPOST SECTION.                                          
034400     SKIP3                                                                
034500     WRITE UTPOST2 FROM UTPOST2-WS                                        
034600                                                                          
034700     MOVE 'W22181'   TO POSTSUM-FDNAMN                                    
034800     MOVE 'W22180D1' TO POSTSUM-DDNAMN2                                   
034900     CALL POSTSUM USING POSTSUM-PARM                                      
035000     .                                                                    
035100     EJECT                                                                
035200 Z-FINIT   SECTION.                                                       
035300     SKIP3                                                                
035400     CLOSE  W22181                                                        
035500            W22182                                                        
035600*- - - - - - - - - - - - - - -  SKRIV UT ANTAL LÄSTA OCH                  
035700*                               SKRIVNA POSTER                            
035800     MOVE 'S' TO POSTSUM-OPKOD                                            
035900     CALL POSTSUM USING POSTSUM-PARM                                      
036000     .                                                                    
036100     EJECT                                                                
036200*    -COPY WY2000Q1                                                       
