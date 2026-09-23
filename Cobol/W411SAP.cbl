000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W411SAP.                                                 
000500 AUTHOR.         UNKNOWN.                                                 
000600 DATE-WRITTEN.   UNKNOWN.                                                 
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001000*        PROGRAMMET ÄR EN SUBMODUL TILL DIVERSE MPP-PGM                   
001100*                                                                         
001200*    FUNKTION.                                                            
001300*      - OM KDCALL = 1 KONTROLLERAR PGM                                   
001400*           .FTGKOD                                                       
001500*           .KONTO                                                        
001600*           .KOSTNADSSTÄLLE                                               
001700*           .RESULTATENHET                                                
001800*           .ANALYSNR                                                     
001900*       (KDCALL = 1 SKALL ENDAST ANVÄNDS I ORDERPGM TYP W40211)           
002000*                                                                         
002100*      - OM KDCALL = 2 KONTROLLERAR PGM                                   
002200*           .KONTO                                                        
002300*           .KOSTNADSSTÄLLE                                               
002400*           .RESULTATENHET                                                
002500*           .ANALYSNR                                                     
002600*                                                                         
002700*        SOM SKICKAS MED AV ANROPANDE PROGRAM                             
002800*                                                                         
002900*    LÄNKAREA: W411SAP                                                    
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400                                                                          
003500 DATA DIVISION.                                                           
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003800*    -- CHECKED BY WY2000                                                 
003900 77  IDPGM                       PIC X(08)   VALUE 'W411SAP'.             
004000                                                                          
004100 77  NEJ                         PIC X(1)    VALUE 'N'.                   
004200 77  JA                          PIC X(1)    VALUE 'J'.                   
004300 77  SPAR-FLKST                  PIC X(1)    VALUE ' '.                   
004400 77  SPAR-FLANALYS               PIC X(1)    VALUE ' '.                   
004500     EJECT                                                                
004600                                                                          
004700 01  NYCKLAR-TILL-DLI.                                                    
004800     03  W-WDH301KY              PIC X(17).                               
004900                                                                          
005000*   NYCKLAR TILL ANALYS             ************                          
005100     03  W-WDH301KY-1.                                                    
005200         05  W-KDSEGKEY-1        PIC X(1)   VALUE '1'.                    
005300         05  W-KDTRADP-1         PIC X(4).                                
005400         05  W-IDANALYS          PIC X(12).                               
005500         05  W-IDANALYS-NUM REDEFINES W-IDANALYS                          
005600                                 PIC 9(12).                               
005700*   NYCKLAR TILL KONTO.             ************                          
005800     03  W-WDH301KY-2.                                                    
005900         05  W-KDSEGKEY-2        PIC X(1)   VALUE '2'.                    
006000         05  W-KDTRADP-2         PIC X(4).                                
006100         05  W-IDKONTO           PIC S9(11) COMP-3.                       
006200         05  FILLER              PIC X(6)   VALUE SPACE.                  
006300                                                                          
006400*   NYCKLAR TILL KOSTNADSSTÄLLE     ************                          
006500     03  W-WDH301KY-3.                                                    
006600         05  W-KDSEGKEY-3        PIC X(1)   VALUE '3'.                    
006700         05  W-KDTRADP-3         PIC X(4).                                
006800         05  W-IDKST             PIC X(10).                               
006900         05  FILLER              PIC X(2)   VALUE SPACE.                  
007000     EJECT                                                                
007100                                                                          
007200 01  TEST-IDDISTR                PIC 9(5)   COMP-3.                       
007300*01 FILLER    -COPY WWDIST18     -RED  TEST-IDDISTR.                      
007400     EJECT                                                                
007500                                                                          
007600*01 FILLER    -COPY WWDIST47     -RED  TEST-IDDISTR.                      
007700     EJECT                                                                
007800                                                                          
007900*01 -COPY WWIDFTG                                                         
008000     EJECT                                                                
008100                                                                          
008200*    ---- ARBETSAREOR FÖR IMS-SECTIONERNA                                 
008300                                                                          
008400 01  FILLER               PIC X(16)  VALUE 'IMS-WS'.                      
008500     SKIP2                                                                
008600*01  -COPY W0003.                                                         
008700     EJECT                                                                
008800 01  GENERELLA-SUBPROGRAM.                                                
008900     03  CBLTDLI          PIC X(8)   VALUE 'CBLTDLI '.                    
009000     03  FELLOG           PIC X(8)   VALUE 'FELLOG  '.                    
009100*    ---- STATUSKOD FRÅN IMS                                              
009200                                                                          
009300 01  STATUS-WS             PIC XX.                                        
009400     88  SEGMENT-FINNS                 VALUE '  '.                        
009500     88  SEGMENT-SAKNAS                VALUE 'GE'.                        
009600     SKIP2                                                                
009700 01  GODK-STATUSKODER.                                                    
009800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009900     SKIP2                                                                
010000 01  SSA1                  PIC X(64).                                     
010100     EJECT                                                                
010200 01  FILLER              PIC X(16) VALUE 'DLI-IO-AREA'.                   
010300     SKIP2                                                                
010400 01  DLI-IO-AREA.                                                         
010500     03  IO-AREA         PIC X(37).                                       
010600                                                                          
010700*    03  WLSAPC01  -COPY WDH301  -PRE SAPC- -RED IO-AREA                  
010800     EJECT                                                                
010900*    -COPY W411SAP      -PRE WS-.                                         
011000     EJECT                                                                
011100 LINKAGE SECTION.                                                         
011200*                                                                         
011300*    -COPY W411SAP                                                        
011400*                                                                         
011500     EJECT                                                                
011600*01  -COPY W0008        -PRE  SAPC-                                       
011700     05  FILLER          PIC X(1).                                        
011800     EJECT                                                                
011900                                                                          
012000 PROCEDURE DIVISION  USING SAP-W411SAP SAPC-PCB.                          
012100     PERFORM A-INIT                                                       
012200                                                                          
012300     IF SAP-KDCALL = +1                                                   
012400       PERFORM B-KONTROLLERA-FTG                                          
012500       PERFORM C-KONTROLLERA-KONTO                                        
012600       IF WS-SAP-IDKONTO-OK = JA                                          
012700         PERFORM D-KONTROLLERA-KST                                        
012800         PERFORM E-KONTROLLERA-PROFIT                                     
012900         PERFORM F-KONTROLLERA-ANALYS                                     
013000         PERFORM G-SAMBAND                                                
013100       END-IF                                                             
013200     END-IF                                                               
013300     IF SAP-KDCALL = +2                                                   
013400       PERFORM C-KONTROLLERA-KONTO                                        
013500       IF WS-SAP-IDKONTO-OK = JA                                          
013600         PERFORM D-KONTROLLERA-KST                                        
013700         PERFORM E-KONTROLLERA-PROFIT                                     
013800         PERFORM F-KONTROLLERA-ANALYS                                     
013900         PERFORM G-SAMBAND                                                
014000       END-IF                                                             
014100     END-IF                                                               
014200                                                                          
014300     MOVE WS-SAP-W411SAP       TO SAP-W411SAP                             
014400     GOBACK                                                               
014500     .                                                                    
014600     EJECT                                                                
014700                                                                          
014800 A-INIT SECTION.                                                          
014900     MOVE JA                   TO SAP-IDFTG-OK                            
015000                                  SAP-IDKONTO-OK                          
015100                                  SAP-IDKST-OK                            
015200                                  SAP-IDPROFIT-OK                         
015300                                  SAP-IDANALYS-OK                         
015400     MOVE SPACE                TO SAP-BEFEL                               
015500                                                                          
015600     MOVE SAP-W411SAP          TO WS-SAP-W411SAP                          
015700     .                                                                    
015800     EJECT                                                                
015900                                                                          
016000 B-KONTROLLERA-FTG SECTION.                                               
016100     IF WS-SAP-IDFTG > 0                                                  
016200       MOVE WS-SAP-IDDISTR      TO TEST-IDDISTR                           
016300       MOVE WS-SAP-IDFTG        TO WS-IDFTG                               
016400       IF WS-SAP-KDFAKTYP = 'G' OR WS-SAP-KDFAKTYP = 'N'                  
016500         PERFORM BA-KONTROLLERA-N-FAKTURA                                 
016600         PERFORM BB-KONTROLLERA-INTERNA                                   
016700         IF IDFTG-GODKAEND                                                
016800           CONTINUE                                                       
016900         ELSE                                                             
017000           MOVE NEJ             TO WS-SAP-IDFTG-OK                        
017100         END-IF                                                           
017200       ELSE                                                               
017300         MOVE NEJ               TO WS-SAP-IDFTG-OK                        
017400       END-IF                                                             
017500     ELSE                                                                 
017600       IF WS-SAP-KDFAKTYP = 'N' OR WS-SAP-KDFAKTYP = 'G' OR               
017700         WS-SAP-IDKONTO   > +0  OR WS-SAP-IDKST    > SPACE                
017800         MOVE NEJ               TO WS-SAP-IDFTG-OK                        
017900       ELSE                                                               
018000         MOVE +0                TO WS-SAP-IDFTG                           
018100       END-IF                                                             
018200     END-IF                                                               
018300     .                                                                    
018400     EJECT                                                                
018500 BA-KONTROLLERA-N-FAKTURA SECTION.                                        
018600                                                                          
018700     IF WS-SAP-KDFAKTYP = 'N'                                             
018800       IF IDFTG-PV OR IDFTG-NON-VCC                                       
018900         CONTINUE                                                         
019000       ELSE                                                               
019100         MOVE NEJ               TO WS-SAP-IDFTG-OK                        
019200       END-IF                                                             
019300     END-IF                                                               
019400     .                                                                    
019500     EJECT                                                                
019600 BB-KONTROLLERA-INTERNA SECTION.                                          
019700                                                                          
019800     IF DIST18-SKROT                                                      
019900       CONTINUE                                                           
020000     ELSE                                                                 
020100       IF DIST47-INTERNA-DEL                                              
020200         IF IDFTG-PV OR IDFTG-NON-VCC                                     
020300           CONTINUE                                                       
020400         ELSE                                                             
020500           MOVE NEJ               TO WS-SAP-IDFTG-OK                      
020600         END-IF                                                           
020700       END-IF                                                             
020800     END-IF                                                               
020900     .                                                                    
021000     EJECT                                                                
021100                                                                          
021200 C-KONTROLLERA-KONTO SECTION.                                             
021300     IF WS-SAP-IDKONTO > 0                                                
021400       MOVE WS-SAP-IDKONTO            TO W-IDKONTO                        
021500       MOVE WS-SAP-KDTRADP            TO W-KDTRADP-2                      
021600       MOVE W-WDH301KY-2              TO W-WDH301KY                       
021700       PERFORM IMS-GU-WLSAPC01                                            
021800       IF SEGMENT-SAKNAS                                                  
021900* KONTO MÅSTE FINNAS I WDH3                                               
022000         MOVE 'N'                     TO WS-SAP-IDKONTO-OK                
022100         MOVE 'ACCOUNT MISSING IN R3' TO WS-SAP-BEFEL                     
022200         MOVE '346'                   TO WS-SAP-IDMFSFEL                  
022300       ELSE                                                               
022400         MOVE SAPC-SAP-FLKST          TO SPAR-FLKST                       
022500         MOVE SAPC-SAP-FLANALYS       TO SPAR-FLANALYS                    
022600       END-IF                                                             
022700     ELSE                                                                 
022800* KONTO MÅSTE ANGES                                                       
022900       MOVE 'N'                       TO WS-SAP-IDKONTO-OK                
023000       MOVE 'ACCOUNT MUST BE REG.'    TO WS-SAP-BEFEL                     
023100       MOVE '350'                     TO WS-SAP-IDMFSFEL                  
023200     END-IF                                                               
023300     .                                                                    
023400     EJECT                                                                
023500                                                                          
023600 D-KONTROLLERA-KST SECTION.                                               
023700     IF WS-SAP-IDKST > SPACE                                              
023800       IF SPAR-FLKST = 'N'                                                
023900* KST FÅR EJ ANGES OM KONTOTS FLAGGA FÖR KST ÄR AVSLAGEN                  
024000         MOVE 'N'                          TO WS-SAP-IDKST-OK             
024100         MOVE 'COSTCENTER NOT ALLOWED'     TO WS-SAP-BEFEL                
024200         MOVE '351'                        TO WS-SAP-IDMFSFEL             
024300       ELSE                                                               
024400         MOVE WS-SAP-IDKST                 TO W-IDKST                     
024500         MOVE WS-SAP-KDTRADP               TO W-KDTRADP-3                 
024600         MOVE W-WDH301KY-3                 TO W-WDH301KY                  
024700         PERFORM IMS-GU-WLSAPC01                                          
024800         IF SEGMENT-SAKNAS                                                
024900* KST MÅSTE FINNAS I WDH3                                                 
025000           MOVE 'N'                        TO WS-SAP-IDKST-OK             
025100           MOVE 'COSTCENTER MISSING IN R3' TO WS-SAP-BEFEL                
025200           MOVE '348'                      TO WS-SAP-IDMFSFEL             
025300         END-IF                                                           
025400       END-IF                                                             
025500     END-IF                                                               
025600     .                                                                    
025700     EJECT                                                                
025800                                                                          
025900 E-KONTROLLERA-PROFIT SECTION.                                            
026000                                                                          
026100     .                                                                    
026200     EJECT                                                                
026300                                                                          
026400 F-KONTROLLERA-ANALYS  SECTION.                                           
026500     IF WS-SAP-IDANALYS NOT = SPACE                                       
026600       IF SPAR-FLANALYS = 'N'                                             
026700* ANALYSNR FÅR EJ ANGES OM KONTOTS FLAGGA FÖR ANALYSNR ÄR AVSLAGEN        
026800         MOVE 'N'                           TO WS-SAP-IDANALYS-OK         
026900         MOVE 'ANALYSIS-NO NOT ALLOWED'     TO WS-SAP-BEFEL               
027000         MOVE '352'                         TO WS-SAP-IDMFSFEL            
027100       ELSE                                                               
027200** ADD LEADING ZEROS TO THE ANALYSIS NUMBER USING NUMVAL                  
027300**                   TO READ THE DATABASE                                 
027400         COMPUTE W-IDANALYS-NUM = FUNCTION NUMVAL(WS-SAP-IDANALYS)        
027500         MOVE WS-SAP-KDTRADP                TO W-KDTRADP-1                
027600         MOVE W-WDH301KY-1                  TO W-WDH301KY                 
027700         PERFORM IMS-GU-WLSAPC01                                          
027800         IF SEGMENT-SAKNAS                                                
027900* ANALYSNR MÅSTE FINNAS I WDH3                                            
028000           MOVE 'N'                         TO WS-SAP-IDANALYS-OK         
028100           MOVE 'ANALYSIS-NO MISSING IN R3' TO WS-SAP-BEFEL               
028200           MOVE '347'                       TO WS-SAP-IDMFSFEL            
028300         END-IF                                                           
028400       END-IF                                                             
028500     END-IF                                                               
028600     .                                                                    
028700     EJECT                                                                
028800                                                                          
028900 G-SAMBAND SECTION.                                                       
029000     IF SPAR-FLANALYS = 'J' AND                                           
029100        SPAR-FLKST    = 'J'                                               
029200       IF WS-SAP-IDANALYS = SPACE AND                                     
029300          WS-SAP-IDKST    = SPACE                                         
029400*  MINST ETT AV ALTERNATIVEN KST/ANALYSNR MÅSTE ANGES                     
029500         MOVE 'N'             TO WS-SAP-IDKST-OK                          
029600                                 WS-SAP-IDANALYS-OK                       
029700         MOVE 'ANALYSIS-NO OR COSTCENTER MUST BE REG.'                    
029800                              TO WS-SAP-BEFEL                             
029900         MOVE '352'           TO WS-SAP-IDMFSFEL                          
030000       END-IF                                                             
030100     END-IF                                                               
030200     .                                                                    
030300     EJECT                                                                
030400                                                                          
030500 IMS-GU-WLSAPC01  SECTION.                                                
030600                                                                          
030700     STRING 'WLSAPC01(WDH301KY =' W-WDH301KY ')'                          
030800             DELIMITED BY SIZE INTO SSA1                                  
030900     MOVE '  GE' TO GODK-STATUSKODER                                      
031000     CALL CBLTDLI USING GU SAPC-PCB DLI-IO-AREA SSA1                      
031100     MOVE SAPC-STATUS-CODE  TO STATUS-WS                                  
031200     PERFORM IMS-STATUSKONTROLL                                           
031300     .                                                                    
031400     EJECT                                                                
031500                                                                          
031600 IMS-STATUSKONTROLL SECTION.                                              
031700                                                                          
031800     SET STATUS-IX TO 1                                                   
031900     SEARCH GODK-STATUS                                                   
032000       AT END                                                             
032100         CALL FELLOG                                                      
032200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032300         CONTINUE                                                         
032400     END-SEARCH                                                           
032500     .                                                                    
