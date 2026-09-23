000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5531600.                                                
000300 AUTHOR.         KARL JOHAN HANSSON.                                      
000400*DATE-WRITTEN.   OKT. 1996.                                               
000500*    REMARKS.                                                             
000600*        PROGRAMMET BERÄKNAR BESTÄLLNINGSPRIS FÖR SATSER.                 
000700*        FÖR SAMTLIGA I SATSEN INGÅENDE ARTIKLAR SUMMERAS                 
000800*        ANTAL * INGÅENDE ARTIKELS STANDARDPRIS.                          
000900*        DENNA SUMMA BLIR SATSARTIKELNS BESTÄLNINGSPRIS.                  
001000*        FÖR UPPDATERING AV DETTA BESTÄLLNINGSPRIS PÅ                     
001100*        ARTIKELREGISTRET SKAPAS EN BAKGRUNDSTRANS SOM VIA                
001200*        PROGRAM W55312/14 UPPDATERAR ARTIKELREGISTRET I                  
001300*        BILD 5111.                                                       
001400*                                                                         
001500*        INDATA:  SATSREGISTER OCH ARTIKELREGISTER                        
001600*                                                                         
001700*        UTDATA:  W55316 (R25:OR TILL W55312/14)                          
001710*                                                                         
001720*  E-TRACKER 1286763, MÄRKNING AV PRISÄNDRING PÅ ONDEMANDLISTOR           
001800     EJECT                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000 INPUT-OUTPUT SECTION.                                                    
002100 FILE-CONTROL.                                                            
002200     SELECT W55316  ASSIGN  TO  UT-S-W55316D1.                            
002300 DATA DIVISION.                                                           
002400 FILE SECTION.                                                            
002500                                                                          
002600 FD  W55316                                                               
002700     RECORDING F                                                          
002800     BLOCK 0.                                                             
002900                                                                          
003000*01  POST -COPY W55310 -PRE  UT-  -L.                                     
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400*    -COPY WY2000W1                                                       
003500 77  IDPGM                       PIC X(8)       VALUE 'W5531600'.         
003600 77  NEJ                         PIC X          VALUE 'N'.                
003700 77  JA                          PIC X          VALUE 'J'.                
003800                                                                          
003900 01  WORKAREOR.                                                           
004000     03  SUMMA-KR-PER-SATS       PIC S9(8)V99   COMP-3 VALUE +0.          
004100     03  W-SATS-IDARTNR          PIC 9(8)        VALUE ZERO.              
004200     03  W-REANTPSA              PIC S9(2)V9(3)  COMP-3.                  
004300     03  DAGENS-DATUM            PIC 9(8).                                
004400     03  SATS-STADAT             PIC 9(8).                                
004500     03  SATS-STODAT             PIC 9(8).                                
004600     03  FL-SATSEN-KOMPLETT      PIC X           VALUE 'J'.               
004700                                                                          
004800 01  GENERELLA-SUBPROGRAM.                                                
004900     03  FELLOG                  PIC X(8)        VALUE 'FELLOG'.          
005000     03  CBLTDLI                 PIC X(8)        VALUE 'CBLTDLI'.         
005100     03  POSTSUM                 PIC X(8)        VALUE 'POSTSUM'.         
005200     EJECT                                                                
005300 01  WORKAREA.                                                            
005400*    03  AREA -COPY W55310   -PRE UT-                                     
005500     EJECT                                                                
005600*    -COPY W0005 -PRE POSTSUM-                                            
005700     EJECT                                                                
005800 01  GODK-STATUSKODER.                                                    
005900     03  GODK-STATUS   PIC XX  OCCURS 5  INDEXED BY STATUS-IX.            
006000     SKIP3                                                                
006100 01  STATUS-WS         PIC XX.                                            
006200     88  SEGMENT-FINNS           VALUE '  '.                              
006300     88  SEGMENT-SAKNAS          VALUE 'GE'.                              
006400     88  BASEN-SLUT              VALUE 'GB'.                              
006500     SKIP3                                                                
006600 01  NYCKLAR-TILL-DLI.                                                    
006700     03  W-IDLEVNR-X.                                                     
006800         05  W-IDLEVNR         PIC X(5).                                  
006900     03  W-IDARTNR-X.                                                     
007000         05  W-IDARTNR         PIC S9(9)   COMP-3.                        
007100     SKIP3                                                                
007200 01  SSA.                                                                 
007300     03  SSA1        PIC X(128).                                          
007400     03  SSA2        PIC X(128).                                          
007500     EJECT                                                                
007600*    IMS FUNKTIONSKODER                                                   
007700*01  -COPY W0003.                                                         
007800     SKIP3                                                                
007900*01  ARTC01-AREA  -COPY WDK601                                            
008000     EJECT                                                                
008100*01  ARTC11-AREA  -COPY WDK611                                            
008200     EJECT                                                                
008300*01  AREA         -COPY WDJ101 -PRE SATB01-                               
008400     EJECT                                                                
008500*01  AREA         -COPY WDJ111 -PRE SATB11-                               
008600     EJECT                                                                
008700 LINKAGE SECTION.                                                         
008800                                                                          
008900*01  -COPY W0008 -PRE ARTC-                                               
009000         05  FILLER          PIC X(10).                                   
009100                                                                          
009200*01  -COPY W0008 -PRE SATB-                                               
009300         05  FILLER          PIC X(10).                                   
009400     EJECT                                                                
009500 PROCEDURE DIVISION USING ARTC-PCB SATB-PCB.                              
009600 MAIN SECTION.                                                            
009700     ENTRY 'DLITCBL' USING ARTC-PCB SATB-PCB.                             
009800                                                                          
009900     PERFORM A-INITIERING                                                 
010000     PERFORM B-BEARBETNING                                                
010100     PERFORM C-AVSLUTNING                                                 
010200                                                                          
010300     MOVE ZERO TO RETURN-CODE                                             
010400     GOBACK                                                               
010500     .                                                                    
010600     EJECT                                                                
010700 A-INITIERING SECTION.                                                    
010800                                                                          
010900     OPEN OUTPUT W55316                                                   
011000                                                                          
011100     MOVE  FUNCTION  CURRENT-DATE(1:8) TO DAGENS-DATUM                    
011200     DISPLAY 'DAGENS DATUM ' DAGENS-DATUM                                 
011300                                                                          
011400     MOVE '985'             TO UT-IDPTYP                                  
011600     MOVE '1002 '           TO UT-IDLEVNR                                 
011800     MOVE '1'               TO UT-KDANTENH                                
012000**   MOVE '0'               TO UT-KDTIPPR-SI                              
012100     MOVE SPACE             TO UT-KDFPKPRI                                
012200     MOVE DAGENS-DATUM(3:6) TO UT-TIPRLIST                                
012300     MOVE 'SEK'             TO UT-KDVALISO                                
012310     MOVE 'W55316  '        TO UT-IDUSER                                  
012400                                                                          
012500     MOVE '1002'            TO W-IDLEVNR                                  
012600     .                                                                    
012700     EJECT                                                                
012800 B-BEARBETNING SECTION.                                                   
012900                                                                          
013000     PERFORM IMS-GET-NEXT-SATB01                                          
013100     PERFORM UNTIL BASEN-SLUT OR SEGMENT-SAKNAS                           
013200       IF SATB01-STR-IDARTNR < 900000000  AND                             
013300          SATB01-STR-TIBORT = 0                                           
013400         MOVE SATB01-STR-IDARTNR      TO W-SATS-IDARTNR                   
013500         MOVE +0                      TO SUMMA-KR-PER-SATS                
013600         MOVE JA                      TO FL-SATSEN-KOMPLETT               
013700                                                                          
013800         PERFORM IMS-GET-SATB11                                           
013900         PERFORM UNTIL SEGMENT-SAKNAS                                     
014000            MOVE SATB11-RAD-TISTADAT  TO SATS-STADAT(2:7)                 
014100            IF SATS-STADAT(3:2) > 50                                      
014200              MOVE 19                 TO SATS-STADAT(1:2)                 
014300            ELSE                                                          
014400              MOVE 20                 TO SATS-STADAT(1:2)                 
014500            END-IF                                                        
014600                                                                          
014700            MOVE SATB11-RAD-TISTODAT  TO SATS-STODAT(2:7)                 
014800            IF SATS-STODAT(3:2) > 50                                      
014900              MOVE 19                 TO SATS-STODAT(1:2)                 
015000            ELSE                                                          
015100              MOVE 20                 TO SATS-STODAT(1:2)                 
015200            END-IF                                                        
015300            IF SATB11-RAD-TISTODAT = +999999                              
015400              MOVE 20999999           TO SATS-STODAT                      
015500            END-IF                                                        
015600                                                                          
015700           IF SATS-STADAT <= DAGENS-DATUM AND                             
015800              SATS-STODAT >  DAGENS-DATUM                                 
015900             MOVE SATB11-RAD-IDARTNR  TO W-IDARTNR                        
016000             MOVE SATB11-RAD-REANTPSA TO W-REANTPSA                       
016100             PERFORM IMS-GET-ARTC01                                       
016200             IF SEGMENT-FINNS                                             
016300               PERFORM IMS-GET-ARTC11                                     
016400               IF SEGMENT-FINNS                                           
016500                 COMPUTE SUMMA-KR-PER-SATS = SUMMA-KR-PER-SATS +          
016600                                    W-REANTPSA * CLAG-PRARTSTD            
016700               ELSE                                                       
016800                 MOVE NEJ             TO FL-SATSEN-KOMPLETT               
016900               END-IF                                                     
017000             ELSE                                                         
017100               MOVE NEJ               TO FL-SATSEN-KOMPLETT               
017200             END-IF                                                       
017300           END-IF                                                         
017400           PERFORM IMS-GET-SATB11                                         
017500         END-PERFORM                                                      
017600                                                                          
017700         IF FL-SATSEN-KOMPLETT = JA                                       
017800           MOVE W-SATS-IDARTNR         TO W-IDARTNR                       
017900           PERFORM IMS-GET-ARTC01                                         
018000           IF SEGMENT-FINNS                                               
018100             PERFORM IMS-GET-ARTC11                                       
018200             IF SEGMENT-FINNS                                             
018300               IF CLAG-KDERS < +9                                         
018400                 COMPUTE UT-PRARTBEL = 100 * SUMMA-KR-PER-SATS            
018500                 PERFORM S01-SKRIV-985                                    
018600               END-IF                                                     
018700             END-IF                                                       
018800           END-IF                                                         
018900         END-IF                                                           
019000       END-IF                                                             
019100       PERFORM IMS-GET-NEXT-SATB01                                        
019200     END-PERFORM                                                          
019300     .                                                                    
019400     EJECT                                                                
019500 S01-SKRIV-985 SECTION.                                                   
019600                                                                          
019700     MOVE W-SATS-IDARTNR    TO UT-IDARTNR                                 
019800     WRITE UT-POST FROM UT-AREA                                           
019900                                                                          
020000     MOVE '985'             TO POSTSUM-TRANSTYP                           
020100     MOVE 'W55316'          TO POSTSUM-FDNAMN                             
020200     MOVE 'W55316D1'        TO POSTSUM-DDNAMN2                            
020300     CALL POSTSUM USING POSTSUM-PARM                                      
020400     .                                                                    
020500     SKIP3                                                                
020600 C-AVSLUTNING SECTION.                                                    
020700                                                                          
020800     MOVE 'S'        TO POSTSUM-OPKOD                                     
020900     CALL POSTSUM USING POSTSUM-PARM                                      
021000                                                                          
021100     CLOSE W55316                                                         
021200     .                                                                    
021300     EJECT                                                                
021400 IMS-GET-NEXT-SATB01 SECTION.                                             
021500                                                                          
021600     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-X ')'                         
021700        DELIMITED BY SIZE INTO SSA1                                       
021800     MOVE '  GBGE' TO GODK-STATUSKODER                                    
021900     CALL CBLTDLI USING  GN  SATB-PCB SATB01-AREA SSA1                    
022000     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
022100     PERFORM IMS-STATUSKONTROLL                                           
022200     .                                                                    
022300     SKIP2                                                                
022400 IMS-GET-SATB11 SECTION.                                                  
022500                                                                          
022600     MOVE 'WLSATB11 ' TO SSA1                                             
022700     MOVE '  GE' TO GODK-STATUSKODER                                      
022800     CALL CBLTDLI USING GNP SATB-PCB SATB11-AREA SSA1                     
022900     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
023000     PERFORM IMS-STATUSKONTROLL                                           
023100     .                                                                    
023200     EJECT                                                                
023300 IMS-GET-ARTC01 SECTION.                                                  
023400                                                                          
023500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
023600                     DELIMITED BY SIZE INTO SSA1                          
023700     MOVE '  GE' TO GODK-STATUSKODER                                      
023800     CALL CBLTDLI USING GU ARTC-PCB ARTC01-AREA SSA1                      
023900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
024000     PERFORM IMS-STATUSKONTROLL                                           
024100     .                                                                    
024200     SKIP2                                                                
024300 IMS-GET-ARTC11 SECTION.                                                  
024400                                                                          
024500     MOVE 'WLARTC11 ' TO SSA1                                             
024600     MOVE '  GE' TO GODK-STATUSKODER                                      
024700     CALL CBLTDLI USING GNP ARTC-PCB ARTC11-AREA SSA1                     
024800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
024900     PERFORM IMS-STATUSKONTROLL                                           
025000     .                                                                    
025100     SKIP2                                                                
025200 IMS-STATUSKONTROLL SECTION.                                              
025300                                                                          
025400     SET STATUS-IX TO 1                                                   
025500     SEARCH GODK-STATUS                                                   
025600       AT END                                                             
025700         CALL FELLOG                                                      
025800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
025900         CONTINUE                                                         
026000     END-SEARCH                                                           
026100     .                                                                    
