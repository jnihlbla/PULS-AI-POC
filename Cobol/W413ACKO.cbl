000010*COMPOPT STDSUB=YES                                                       
000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W413ACKO.                                                
000400 AUTHOR.         CAMELIA OLGRENER.                                        
000500 DATE-WRITTEN.   MAJ-90.                                                  
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*        DETTA ÄR ETT GENERELLT SUBPROGRAM SOM INGÅR                      
001000*        I WOPS.                                                          
001100*                                                                         
001200*    FUNKTION.                                                            
001300*        PGM-ET BERÄKNAR DEN DEL AV ACKORDTIDEN SOM                       
001400*        BEROR PÅ LAGEROMRÅDET.                                           
001500*                                                                         
001600*        BERÄKNINGEN SKER EMBART M A P LAGEROMRÅDE.                       
001700*                                                                         
001800*        ENDAST C1-O-DEL MED KLASS 4 I LAGEROMR: 10;                      
001900*        20; 25; 30 OCH 35 BERÄKNAS.                                      
002000*                                                                         
002100*                                                                         
002200*    LÄNKAREA :    W413ACKOC0                                             
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800 WORKING-STORAGE SECTION.                                                 
002801                                                                          
002810*    -- CHECKED BY WY2000                                                 
002900 77  IDPGM                       PIC X(08)       VALUE 'W413ACKO'.        
003000 77  JA                          PIC X           VALUE 'J'.               
003100 77  NEJ                         PIC X           VALUE 'N'.               
003200                                                                          
003300 77  FAST-TID-PER-RAD            PIC 9(8)              VALUE ZERO.        
003400 77  FAST-ORDER-TID              PIC 9(5)              VALUE ZERO.        
003500 77  FAST-RAD-TID                PIC 9(4)              VALUE ZERO.        
003600 77  TRP-TID                     PIC 9(6)              VALUE ZERO.        
003700 77  PROCENT-FORD                PIC 9(3)              VALUE ZERO.        
003800 77  TOT-KVRADER                 PIC S9(5)             VALUE ZERO.        
003900 77  SOEK-KVRADER                PIC S9(5)             VALUE ZERO.        
004000 77  ANSLAGEN-TID                PIC S9(15)     COMP-3 VALUE ZERO.        
004100 77  TOTALT-ANSLAGEN-TID         PIC S9(8)V9(6) COMP-3 VALUE ZERO.        
004200                                                                          
004300    EJECT                                                                 
004400*   -COPY W413KRAV                                                        
004600    EJECT                                                                 
004700                                                                          
004800*   -COPY W415R96                                                         
005000    EJECT                                                                 
005100                                                                          
005200*   -COPY W413TRPF                                                        
005400    EJECT                                                                 
005500                                                                          
005600*   -COPY W415R97                                                         
005800    EJECT                                                                 
005900                                                                          
006000*   -COPY W413TRPG                                                        
006200    EJECT                                                                 
006300                                                                          
006400*   -COPY W415R98                                                         
006600    EJECT                                                                 
006700                                                                          
006800*   -COPY W413FORD                                                        
007000    EJECT                                                                 
007100                                                                          
007200*   -COPY W415R99                                                         
007400    EJECT                                                                 
007500                                                                          
007600 01  GENERELLA-SUBPROGRAM.                                                
007700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007900                                                                          
008000                                                                          
008100 LINKAGE SECTION.                                                         
008200*                                                                         
008300*   -COPY W413ACKO                                                        
008500*                                                                         
008600     EJECT                                                                
008700 PROCEDURE DIVISION  USING ACKO-W413ACKO.                                 
008800                                                                          
008900     IF ACKO-ADLAGOMR = +10 OR +21 OR +25 OR +30 OR +35                   
009000                                                                          
009100       MOVE ZERO TO ACKO-TIHANTTI                                         
009200                                                                          
009300       PERFORM S01-LAES-FRAM-FDATAKOD                                     
009400                                                                          
009500       IF ACKO-ADLAGOMR = +10 OR +21 OR +25                               
009600         PERFORM S02-LAES-FRAM-TRPTID-FACK-PALL                           
009700       END-IF                                                             
009800                                                                          
009900       IF ACKO-ADLAGOMR = +30 OR +35                                      
010000         PERFORM A-KOLLA-KVRADER                                          
010100         PERFORM S03-LAES-FRAM-TRPTID-GROV                                
010200       END-IF                                                             
010300                                                                          
010400       PERFORM B-BERAEKNA-ACKORDTID                                       
010500       MOVE TOTALT-ANSLAGEN-TID TO ACKO-TIHANTTI                          
010600                                                                          
010700     END-IF                                                               
010800                                                                          
010900     GOBACK                                                               
011000     .                                                                    
011100     EJECT                                                                
011200 A-KOLLA-KVRADER SECTION.                                                 
011300                                                                          
011400************************************************************              
011500*   VID BRYTNING INOM SAMMA ORDER AV ORDERKLASS 4          *              
011600*   MELLAN LAGEROMRÅDE 30 OCH 35 GÄLLER FÖLJANDE:          *              
011700*                                                          *              
011800*   - OM LAGEROMRÅDE ÄR 35 OCH FÖREGÅENDE ÄR 30 ADDERAR    *              
011900*     MAN ANTAL ARTIKELNR FÖR LAGEROMRÅDE 30 (ACKO-FOERRA- *              
012000*     KVRADER) MED ANTAL ARTIKELNR FÖR LAGEROMRÅDE 35      *              
012100*     (ACKO-KVRADER).                                      *              
012200*                                                          *              
012300*   - TOTALEN (TOT-KVRADER) ANVÄNDER MAN VID SÖKNING AV    *              
012400*     TRANSPORTTID (TRP-TID) FÖR LAGEROMRÅDE 35.           *              
012500*     DET SAMMANLAGDA TRANSPORTTIDEN SOM MAN BEHÖVER FÖR   *              
012600*     BÅDE LAGEROMRÅDE 30 OCH 35 ANVÄNDER MAN I BERÄKNING  *              
012700*     AV TOTALT ANSLAGEN TID / ORDER.                      *              
012800*                                                          *              
012900*   ORSAKEN ATT MAN GÖR SÅ ÄR ATT DESSA TVÅ LAGEROMRÅDEN   *              
013000*   LIGGER NÄRA VARANDRA OCH DÅ BEHÖVER MAN BARA EN        *              
013100*   SAMMANLAGD TRANSPORTTID FÖR LAGEROMRÅDE 30 OCH 35.     *              
013200************************************************************              
013300                                                                          
013400     IF ACKO-ADLAGOMR      = +35 AND                                      
013500        ACKO-FOERRA-ADLAGOMR = +30                                        
013600                                                                          
013700       COMPUTE TOT-KVRADER = ACKO-FOERRA-KVRADER + ACKO-KVRADER           
013800       MOVE TOT-KVRADER TO SOEK-KVRADER                                   
013900                                                                          
014000     ELSE                                                                 
014100       MOVE ACKO-KVRADER TO SOEK-KVRADER                                  
014200     END-IF                                                               
014300     .                                                                    
014400     EJECT                                                                
014500 B-BERAEKNA-ACKORDTID SECTION.                                            
014600                                                                          
014700     PERFORM BA-KOLLA-VILKET-LAGOMR                                       
014800     COMPUTE FAST-TID-PER-RAD = FAST-RAD-TID                              
014900                                       * ACKO-KVRADER                     
015000                                                                          
015100     PERFORM BB-KOLLA-OM-35LAGOMR-BRYTN                                   
015200     COMPUTE ANSLAGEN-TID = FAST-ORDER-TID                                
015300                           + TRP-TID                                      
015400                           + FAST-TID-PER-RAD                             
015500                                                                          
015600     ADD ACKO-SUHANTTI TO ANSLAGEN-TID                                    
015700                                                                          
015800     PERFORM BC-KOLLA-VILKET-DISTRIKT                                     
015900     COMPUTE ANSLAGEN-TID = ANSLAGEN-TID                                  
016000                           + (ANSLAGEN-TID * PROCENT-FORD)                
016100                           / 1000                                         
016200                                                                          
016300     COMPUTE TOTALT-ANSLAGEN-TID  = ANSLAGEN-TID                          
016400                                   / 100000                               
016500     .                                                                    
016600     EJECT                                                                
016700 BA-KOLLA-VILKET-LAGOMR SECTION.                                          
016800                                                                          
016900     IF ACKO-ADLAGOMR = +10                                               
017000       MOVE FD-TIFTR-F TO FAST-RAD-TID                                    
017100       MOVE FD-TIFTO-F TO FAST-ORDER-TID                                  
017200       MOVE TF-TITRP-F TO TRP-TID                                         
017300     END-IF                                                               
017400                                                                          
017500     IF ACKO-ADLAGOMR = +21 OR +25                                        
017600       MOVE FD-TIFTR-P TO FAST-RAD-TID                                    
017700       MOVE FD-TIFTO-P TO FAST-ORDER-TID                                  
017800       MOVE TF-TITRP-P TO TRP-TID                                         
017900     END-IF                                                               
018000                                                                          
018100     IF ACKO-ADLAGOMR = +30                                               
018200       MOVE FD-TIFTR-G1 TO FAST-RAD-TID                                   
018300       MOVE FD-TIFTO-G  TO FAST-ORDER-TID                                 
018400       MOVE TG-TITRP-G  TO TRP-TID                                        
018500     END-IF                                                               
018600                                                                          
018700     IF ACKO-ADLAGOMR = +35                                               
018800       MOVE FD-TIFTR-G2 TO FAST-RAD-TID                                   
018900       MOVE FD-TIFTO-G  TO FAST-ORDER-TID                                 
019000       MOVE TG-TITRP-G  TO TRP-TID                                        
019100     END-IF                                                               
019200     .                                                                    
019300     EJECT                                                                
019400 BB-KOLLA-OM-35LAGOMR-BRYTN SECTION.                                      
019500                                                                          
019600**************************************************************            
019700*    VID BRYTNING INOM SAMMA ORDER AV ORDERKLASS 4           *            
019800*    MELLAN LAGEROMRÅDE 30 OCH 35 GÄLLER FÖLJANDE:           *            
019900*                                                            *            
020000*    - OM LAGEROMRÅDE 30 FÖLJS AV LAGEROMRÅDE 35 SÄTTER      *            
020100*      MAN FAST TID PER ORDER (FAST-ORDER-TID) OCH           *            
020200*      TRANSPORTTID (TRP-TID) FÖR LAGEROMRÅDE 30 TILL 0.     *            
020300*      DETTA FÖR ATT NÄR MAN BEHANDLAR LAGEROMRÅDE 35        *            
020400*      BERÄKNAR MAN DEN SAMMANLAGDA TRANSPORTTIDEN OCH       *            
020500*      FAST TID PER ORDER FÖR LAGEROMRÅDE 30 OCH 35.         *            
020600*                                                            *            
020700*    ORSAKEN ATT MAN GÖR SÅ ÄR ATT DESSA TVÅ LAGEROMRÅDEN    *            
020800*    LIGGER NÄRA VARANDRA OCH DÅ BEHÖVER MAN BARA EN FAST    *            
020900*    TID PER ORDER OCH EN TRANSPORTTID.                      *            
021000**************************************************************            
021100                                                                          
021200     IF ACKO-ADLAGOMR = +30       AND                                     
021300        ACKO-NAESTA-ADLAGOMR = +35                                        
021400                                                                          
021500       MOVE ZERO TO FAST-ORDER-TID                                        
021600                    TRP-TID                                               
021610     END-IF                                                               
021700     .                                                                    
021800     EJECT                                                                
021900 BC-KOLLA-VILKET-DISTRIKT SECTION.                                        
022000                                                                          
022100     IF ACKO-ADLAGOMR = +10                                               
022200       MOVE FORD-TABELLRAD(1) TO FP-W415F1-PRO                            
022300     END-IF                                                               
022400                                                                          
022500     IF ACKO-ADLAGOMR = +21 OR +25                                        
022600       MOVE FORD-TABELLRAD(2) TO FP-W415F1-PRO                            
022700     END-IF                                                               
022800                                                                          
022900     IF ACKO-ADLAGOMR = +30 OR +35                                        
023000       MOVE FORD-TABELLRAD(3) TO FP-W415F1-PRO                            
023100     END-IF                                                               
023200                                                                          
023300     IF ACKO-IDDISTR > +0 AND < +800                                      
023400       MOVE FP-REFORPRO-SWE TO PROCENT-FORD                               
023500     END-IF                                                               
023600                                                                          
023700     IF ACKO-IDDISTR > +799 AND < +1000                                   
023800       MOVE FP-REFORPRO-SCA TO PROCENT-FORD                               
023900     END-IF                                                               
024000                                                                          
024100     IF ACKO-IDDISTR > +999                                               
024200       MOVE FP-REFORPRO-OVR TO PROCENT-FORD                               
024300     END-IF                                                               
024400     .                                                                    
024500     EJECT                                                                
024600 S01-LAES-FRAM-FDATAKOD SECTION.                                          
024700                                                                          
024800     SET KRAV-INDX TO +1                                                  
024900                                                                          
025000     SEARCH KRAV-TABELLRAD                                                
025100       AT END                                                             
025200         SET KRAV-INDX TO +1                                              
025300         MOVE KRAV-TABELLRAD(KRAV-INDX) TO FD-W415F1-KDFD                 
025400       WHEN KRAV-KDFDKRAV(KRAV-INDX) = ACKO-KDFDKRAV                      
025500         MOVE KRAV-TABELLRAD(KRAV-INDX) TO FD-W415F1-KDFD                 
025600     END-SEARCH                                                           
025700                                                                          
025800     .                                                                    
025900     EJECT                                                                
026000 S02-LAES-FRAM-TRPTID-FACK-PALL SECTION.                                  
026100                                                                          
026200     SET TRPF-INDX TO +1                                                  
026300                                                                          
026400     SEARCH TRPF-TABELLRAD                                                
026500       AT END                                                             
026600         SET TRPF-INDX TO +1                                              
026700         MOVE TRPF-TABELLRAD(TRPF-INDX) TO TF-W415F1-TRPFP                
026800       WHEN ACKO-KVRADER NOT > TRPF-KVRADER(TRPF-INDX)                    
026900         MOVE TRPF-TABELLRAD(TRPF-INDX) TO TF-W415F1-TRPFP                
027000     END-SEARCH                                                           
027100                                                                          
027200     .                                                                    
027300     EJECT                                                                
027400 S03-LAES-FRAM-TRPTID-GROV SECTION.                                       
027500                                                                          
027600     SET TRPG-INDX TO +1                                                  
027700                                                                          
027800     SEARCH TRPG-TABELLRAD                                                
027900       AT END                                                             
028000         SET TRPG-INDX TO +1                                              
028100         MOVE TRPG-TABELLRAD(TRPG-INDX) TO TG-W415F1-TRPG                 
028200       WHEN SOEK-KVRADER NOT > TRPG-KVRADER(TRPG-INDX)                    
028300         MOVE TRPG-TABELLRAD(TRPG-INDX) TO TG-W415F1-TRPG                 
028400     END-SEARCH                                                           
028500                                                                          
028600     .                                                                    
