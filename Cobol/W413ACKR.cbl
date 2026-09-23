000010*COMPOPT STDSUB=YES                                                       
000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W413ACKR.                                                
000400 AUTHOR.         CAMELIA OLGRENER.                                        
000500 DATE-WRITTEN.   MAJ-90.                                                  
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*        DETTA ÄR ETT GENERELLT SUBPROGRAM SOM INGÅR I                    
001000*        WOPS.                                                            
001100*                                                                         
001200*    FUNKTION.                                                            
001300*        PGM-ET BERÄKNAR HANTERINGSTIDEN PER RAD FÖR DE                   
001400*        ORDERKLASSER OCH LAGEROMRÅDEN DÄR MAN ANVÄNDER                   
001500*        ACKORD.                                                          
001600*                                                                         
001700*        ENDAST C1-ORDERRADER MED KLASS 4 I LAGEROMR: 10;                 
001800*        20;25;30;35 BERÄKNAS.                                            
001900*                                                                         
002000*                                                                         
002100*    LÄNKAREA :    W413ACKRC0                                             
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700 WORKING-STORAGE SECTION.                                                 
002710                                                                          
002720*  --  CHECKED BY WY2000                                                  
002800 77  IDPGM                       PIC X(08)       VALUE 'W413ACKR'.        
002900 77  JA                          PIC X           VALUE 'J'.               
003000 77  NEJ                         PIC X           VALUE 'N'.               
003100                                                                          
003200 77  KVANT                       PIC S9(5)  VALUE ZERO     COMP-3.        
003300 77  TID-PER-RAD                 PIC S9(15) VALUE ZERO     COMP-3.        
003400 77  TID-HANTKOD                 PIC S9(15) VALUE ZERO     COMP-3.        
003500 77  PACK-TID                    PIC S9(15) VALUE ZERO     COMP-3.        
003600 77  TID-GANGTID-TOT             PIC S9(15) VALUE ZERO     COMP-3.        
003700 77  TID-GANGTID1                PIC S9(5)V9(2) VALUE ZERO COMP-3.        
003800 77  TID-GANGTID2                PIC S9(15) VALUE ZERO     COMP-3.        
003900 77  TID-PER-KOLLI               PIC S9(15) VALUE ZERO     COMP-3.        
004000 77  KOLLIFAKTOR                 PIC 9V9(2) VALUE ZERO.                   
004100 77  TID-KOLLI                   PIC 9(5)   VALUE ZERO.                   
004200                                                                          
004300*77  INDX                        PIC S9(9)  VALUE ZERO  COMP SYNC.        
004400                                                                          
004500    EJECT                                                                 
004600*   -COPY W413HANT                                                        
004800    EJECT                                                                 
004900                                                                          
005000*   -COPY W415R95                                                         
005200    EJECT                                                                 
005300                                                                          
005400*   -COPY W413KRAV                                                        
005600    EJECT                                                                 
005700                                                                          
005800*   -COPY W415R96                                                         
006000    EJECT                                                                 
006100                                                                          
006200 01  GENERELLA-SUBPROGRAM.                                                
006300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006500                                                                          
006600                                                                          
006700 LINKAGE SECTION.                                                         
006800                                                                          
006900*   -COPY W413ACKR                                                        
007100     EJECT                                                                
007200                                                                          
007300 PROCEDURE DIVISION  USING ACKR-W413ACKR.                                 
007400                                                                          
007500     MOVE ZERO TO ACKR-KVHANTTI                                           
007600                                                                          
007700     IF ACKR-ADLAGOMR = +10                                               
007800       PERFORM A-FACKLAG-ART                                              
007900     END-IF                                                               
008000                                                                          
008100     IF ACKR-ADLAGOMR = +21 OR +25 OR +30 OR +35                          
008200       PERFORM B-PALL-O-GROVLAG-ART                                       
008300     END-IF                                                               
008400                                                                          
008500     GOBACK                                                               
008600     .                                                                    
008700     EJECT                                                                
008800 A-FACKLAG-ART SECTION.                                                   
008900                                                                          
009000     PERFORM S02-LAES-FRAM-HANTERINGSKOD                                  
009100                                                                          
009200     IF ACKR-KVQPACK-1 = +0                                               
009300       MOVE +1 TO KVANT                                                   
009400     ELSE                                                                 
009500       MOVE ACKR-KVQPACK-1 TO KVANT                                       
009600     END-IF                                                               
009700                                                                          
009800     PERFORM AA-BERAEKNA-TID-PER-RAD                                      
009900                                                                          
010000     MOVE TID-PER-RAD TO ACKR-KVHANTTI                                    
010100     .                                                                    
010200     EJECT                                                                
010300 AA-BERAEKNA-TID-PER-RAD SECTION.                                         
010400                                                                          
010500     COMPUTE TID-PER-RAD ROUNDED = HK-TIFT-F                              
010600                                  + (ACKR-KVBEART-Q * HK-TIVT-F)          
010700                                  / KVANT                                 
010800     .                                                                    
010900     EJECT                                                                
011000 B-PALL-O-GROVLAG-ART SECTION.                                            
011100                                                                          
011200     PERFORM S01-LAES-FRAM-FDATAKOD                                       
011300                                                                          
011400     PERFORM S02-LAES-FRAM-HANTERINGSKOD                                  
011500                                                                          
011600     IF ACKR-KDARTHNT > +599 AND < +700                                   
011700                                                                          
011800       PERFORM BA-HANTERINGSKOD-6XX                                       
011900     ELSE                                                                 
012000                                                                          
012100       PERFORM BB-HANTERINGSKOD-UTOM-6XX                                  
012200     END-IF                                                               
012300     .                                                                    
012400     EJECT                                                                
012500 BA-HANTERINGSKOD-6XX SECTION.                                            
012600                                                                          
012700     IF ACKR-KVQPACK-1 = +0                                               
012800       MOVE +1 TO ACKR-KVQPACK-1                                          
012900     END-IF                                                               
013000                                                                          
013100     COMPUTE PACK-TID ROUNDED = HK-TITENHL                                
013200                               * ACKR-KVBEART-Q                           
013300                               / ACKR-KVQPACK-1                           
013400                                                                          
013500     MOVE PACK-TID TO ACKR-KVHANTTI                                       
013600     .                                                                    
013700     EJECT                                                                
013800 BB-HANTERINGSKOD-UTOM-6XX SECTION.                                       
013900                                                                          
014000     PERFORM BBA-BERAEKNA-TID-HANTKOD                                     
014100     ADD TID-HANTKOD TO ACKR-KVHANTTI                                     
014200                                                                          
014300     PERFORM BBB-BERAEKNA-GANGTID                                         
014400     ADD TID-GANGTID-TOT TO ACKR-KVHANTTI                                 
014500                                                                          
014600     PERFORM BBC-BERAEKNA-TID-PER-KOLLI                                   
014700     ADD TID-PER-KOLLI TO ACKR-KVHANTTI                                   
014800     .                                                                    
014900     EJECT                                                                
015000 BBA-BERAEKNA-TID-HANTKOD SECTION.                                        
015100                                                                          
015200     IF ACKR-KVQPACK-1 = +0                                               
015300       MOVE +1 TO KVANT                                                   
015400     ELSE                                                                 
015500       MOVE ACKR-KVQPACK-1 TO KVANT                                       
015600     END-IF                                                               
015700                                                                          
015800     COMPUTE TID-HANTKOD ROUNDED = HK-TIFT-PG                             
015900                                  + (ACKR-KVBEART-Q * HK-TIVT-PG)         
016000                                  / KVANT                                 
016100     .                                                                    
016200     EJECT                                                                
016300 BBB-BERAEKNA-GANGTID SECTION.                                            
016400                                                                          
016500     IF HK-KVFLYTT = +0                                                   
016600       MOVE +0 TO TID-GANGTID1                                            
016700     ELSE                                                                 
016800       COMPUTE TID-GANGTID1 ROUNDED = ACKR-KVBEART-Q                      
016900                                     / (HK-KVFLYTT * KVANT)               
017000     END-IF                                                               
017100                                                                          
017200     IF TID-GANGTID1 < +1                                                 
017300       MOVE +1 TO TID-GANGTID2                                            
017400     ELSE                                                                 
017500       COMPUTE TID-GANGTID2 ROUNDED = TID-GANGTID1                        
017600     END-IF                                                               
017700                                                                          
017800     COMPUTE TID-GANGTID-TOT ROUNDED = TID-GANGTID2 * HK-TIGANG           
017900     .                                                                    
018000     EJECT                                                                
018100 BBC-BERAEKNA-TID-PER-KOLLI SECTION.                                      
018200                                                                          
018300     IF HK-KVTRPEMB = +0                                                  
018400       MOVE +2000 TO HK-KVTRPEMB                                          
018500     END-IF                                                               
018600                                                                          
018700*** 2000 ÄR ETT MEDELVÄRDE FÖR ANTAL DETALJER/EMBALLAGE) ***              
018800                                                                          
018900     IF ACKR-ADLAGOMR = +21 OR +25                                        
019000       MOVE FD-REKLI-P TO KOLLIFAKTOR                                     
019100       MOVE FD-TIKLI-P TO TID-KOLLI                                       
019200     ELSE                                                                 
019300       IF ACKR-ADLAGOMR = +30 OR +35                                      
019400         MOVE FD-REKLI-G TO KOLLIFAKTOR                                   
019500         MOVE FD-TIKLI-G TO TID-KOLLI                                     
019600       END-IF                                                             
019700     END-IF                                                               
019800                                                                          
019900     COMPUTE TID-PER-KOLLI ROUNDED = ACKR-KVBEART-Q                       
020000                                    * KOLLIFAKTOR                         
020100                                    * TID-KOLLI                           
020200                                    / (HK-KVTRPEMB * KVANT)               
020300     .                                                                    
020400     EJECT                                                                
020500 S01-LAES-FRAM-FDATAKOD SECTION.                                          
020600                                                                          
020700     SET KRAV-INDX TO +1                                                  
020800                                                                          
020900     SEARCH KRAV-TABELLRAD                                                
021000       AT END                                                             
021100         SET KRAV-INDX TO +1                                              
021200         MOVE KRAV-TABELLRAD(KRAV-INDX) TO FD-W415F1-KDFD                 
021300       WHEN KRAV-KDFDKRAV(KRAV-INDX) = ACKR-KDFDKRAV                      
021400         MOVE KRAV-TABELLRAD(KRAV-INDX) TO FD-W415F1-KDFD                 
021500     END-SEARCH                                                           
021600                                                                          
021700     .                                                                    
021800     EJECT                                                                
021900 S02-LAES-FRAM-HANTERINGSKOD SECTION.                                     
022000                                                                          
022100     IF ACKR-KDARTHNT = +0                                                
022200       IF ACKR-ADLAGOMR = +10 OR +21 OR +25                               
022300         MOVE +110 TO ACKR-KDARTHNT                                       
022400       ELSE                                                               
022500         MOVE +190 TO ACKR-KDARTHNT                                       
022600       END-IF                                                             
022700     END-IF                                                               
022800                                                                          
022900*** 110 OCH 190 ÄR MEDELHANTERINGSKODER/ LAGEROMRÅDE ***                  
023000                                                                          
023100     SET HANT-INDX TO +1                                                  
023200                                                                          
023300     SEARCH HANT-TABELLRAD                                                
023400       AT END                                                             
023500         SET HANT-INDX TO +1                                              
023600         MOVE HANT-TABELLRAD(HANT-INDX) TO HK-W415F1-KDHNT                
023700       WHEN HANT-KDARTHNT(HANT-INDX) = ACKR-KDARTHNT                      
023800         MOVE HANT-TABELLRAD(HANT-INDX) TO HK-W415F1-KDHNT                
023900     END-SEARCH                                                           
024000                                                                          
024100     .                                                                    
