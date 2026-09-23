000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W411LAST.                                                
000500 AUTHOR.         LARS THELL CAP GEMINI LOCIC.                             
000600 DATE-WRITTEN.   MAJ   -90.                                               
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001000*        PROGRAMMET ÄR EN SUBMODUL TILL ETT MPP-PGM                       
001100*                                                                         
001200*    FUNKTION.                                                            
001300*      - KONTROLL OM ENHETSLAST. ENHETSLAST ÄR ATT                        
001400*        VISS DEL AV DET BESTÄLLDA ANTALET TAS IFRÅN                      
001500*        DEN NORMALA PLOCKPLATSEN OCH VISS DEL IFRÅN                      
001600*        ANNAN LAGERPLATS. DET KAN DÅ VARA FRÅGAN OM                      
001700*        HEL PALL.                                                        
001800*                                                                         
001900*        LÄNKAREA: W411LAST                                               
002000*                                                                         
002100* ETRACKER 3030306 CRITERIA FOR UNIT LOAD - FEB. 2006                     
002200     EJECT                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP3                                                                
002500 DATA DIVISION.                                                           
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800*    -- CHECKED BY WY2000                                                 
002900 77  IDPGM                       PIC X(08)   VALUE 'W411LAST'.            
003000 77  JA                          PIC X       VALUE 'J'.                   
003100 77  NEJ                         PIC X       VALUE 'N'.                   
003200 77  W-ANT-ENHETSLAST            PIC S9(7)     COMP-3  VALUE ZERO.        
003300 77  W-ENHETSLAST-REST           PIC S9(7)     COMP-3  VALUE ZERO.        
003400                                                                          
003500 77  ENHETSLAST-SW               PIC X       VALUE 'J'.                   
003600     88 ENHETSLAST                           VALUE 'J'.                   
003700*      --- VALID IDDC CODES                                               
003800*                                                                         
003900*01    -COPY WWDC99                                                       
004000       EJECT                                                              
004100     EJECT                                                                
004200 LINKAGE SECTION.                                                         
004300*                                                                         
004400*   -COPY W411LAST                                                        
004500*                                                                         
004600     EJECT                                                                
004700 PROCEDURE DIVISION  USING LAST-W411LAST.                                 
004800                                                                          
004900 STYR SECTION.                                                            
005000                                                                          
005100     PERFORM A-INIT                                                       
005200                                                                          
005300     IF LAST-FLFORBI = NEJ AND LAST-FLORDSPE = NEJ AND                    
005400        LAST-FLOVRLEV = NEJ                                               
005500                                                                          
005600       PERFORM B-KONTR-IDLEVNR                                            
005700                                                                          
005800       IF ENHETSLAST                                                      
005900          PERFORM C-SAETT-ENHETSLAST                                      
006000          IF ENHETSLAST                                                   
006100              PERFORM D-FORANDRA-LAGEROMR                                 
006200          END-IF                                                          
006300       END-IF                                                             
006400     END-IF                                                               
006500                                                                          
006600     GOBACK                                                               
006700     .                                                                    
006800     EJECT                                                                
006900                                                                          
007000 A-INIT          SECTION.                                                 
007100                                                                          
007200     MOVE JA                   TO ENHETSLAST-SW                           
007300     MOVE +0                   TO LAST-ADGANG-UT                          
007400                                  LAST-ADLAGOMR-UT                        
007500                                  LAST-KVANTAL-UT                         
007600                                  LAST-KVBEART-UT                         
007700                                  W-ANT-ENHETSLAST                        
007800                                  W-ENHETSLAST-REST                       
007900     .                                                                    
008000     EJECT                                                                
008100                                                                          
008200 B-KONTR-IDLEVNR SECTION.                                                 
008300                                                                          
008400     IF LAST-IDLEVNR           NOT = SPACE                                
008500         MOVE NEJ              TO ENHETSLAST-SW                           
008600     END-IF                                                               
008700     .                                                                    
008800     EJECT                                                                
008900                                                                          
009000 C-SAETT-ENHETSLAST  SECTION.                                             
009100                                                                          
009200     MOVE LAST-IDDC              TO WS-IDDC                               
009300     IF CDC-SE                   AND                                      
009400       (LAST-KVQPACK-3    > +1   OR                                       
009500        LAST-KVQPACK-4    > +0)                                           
009600         IF LAST-KDFDKRAV = +0 OR +5 OR +6 OR +7 OR +8 OR                 
009700                            +9 OR +10 OR +11 OR +12 OR +13 OR             
009800                            +14 OR +17                                    
009900             MOVE NEJ     TO ENHETSLAST-SW                                
010000          ELSE                                                            
010100             IF LAST-KDFDKRAV = +1 OR +02 OR +03 OR +04 OR                
010200                                +15 OR +16 OR                             
010300  +22 OR +23 OR +24 OR +25 OR +26 OR +27 OR +28 OR +29 OR                 
010400  +30 OR +31 OR +32 OR +33 OR +34 OR +35 OR +36 OR +37 OR +38 OR          
010500  +39 OR                                                                  
010600  +40 OR +41 OR +42 OR +43 OR +44 OR +45 OR +46 OR +47 OR +48 OR          
010700  +49 OR                                                                  
010800  +50 OR +51 OR +52 OR +53 OR +54 OR +55 OR +56 OR +57 OR +58 OR          
010900  +59 OR                                                                  
011000  +60 OR +61 OR +62 OR +63 OR +64 OR +65 OR +66 OR +67 OR +68 OR          
011100  +69 OR                                                                  
011200  +70 OR +71 OR +72 OR +73 OR +74 OR +75 OR +76 OR +77 OR +78 OR          
011300  +79 OR                                                                  
011400  +80 OR +81 OR +82 OR +83 OR +84 OR +85 OR +86 OR +87 OR +88 OR          
011500  +89 OR                                                                  
011600  +90 OR +91 OR +92 OR +93 OR +94 OR +95 OR +96 OR +97 OR +98 OR          
011700  +99                                                                     
011800                                                                          
011900                 IF LAST-KVQPACK-3 > +1                                   
012000                   COMPUTE W-ANT-ENHETSLAST =                             
012100                         LAST-KVPREAVB / LAST-KVQPACK-3                   
012200                   END-COMPUTE                                            
012300                   COMPUTE W-ANT-ENHETSLAST =                             
012400                         W-ANT-ENHETSLAST * LAST-KVQPACK-3                
012500                   END-COMPUTE                                            
012600                   COMPUTE W-ENHETSLAST-REST =                            
012700                         LAST-KVPREAVB -  W-ANT-ENHETSLAST                
012800                   END-COMPUTE                                            
012900                 END-IF                                                   
013000             END-IF                                                       
013100         END-IF                                                           
013200     END-IF                                                               
013300     .                                                                    
013400     EJECT                                                                
013500 D-FORANDRA-LAGEROMR  SECTION.                                            
013600                                                                          
013700     IF (W-ANT-ENHETSLAST = +0 AND W-ENHETSLAST-REST = +0)                
013800        OR                                                                
013900        (LAST-ADLAGOMR = +16 OR +26 OR                                    
014000                         +42 OR +43 OR +44 OR +45 OR                      
014100                         +78 OR +90 OR +91)                               
014200                                                                          
014300****   VANLIGA LAGEROMRÅDET GÄLLER                                        
014400         MOVE +0               TO LAST-KVBEART-UT                         
014500         MOVE +0               TO LAST-KVANTAL-UT                         
014600         MOVE +0               TO LAST-ADLAGOMR-UT                        
014700         MOVE +0               TO LAST-ADGANG-UT                          
014800     ELSE                                                                 
014900       IF W-ANT-ENHETSLAST       >  +0 AND                                
015000         W-ENHETSLAST-REST      = +0                                      
015100         EVALUATE LAST-ADLAGOMR                                           
015200           WHEN +10                                                       
015300             MOVE LAST-ADLAGOMR TO LAST-ADLAGOMR-UT                       
015400           WHEN +11                                                       
015500             MOVE +63          TO LAST-ADLAGOMR-UT                        
015600           WHEN +14 THRU +15                                              
015700             MOVE +62          TO LAST-ADLAGOMR-UT                        
015800           WHEN +20                                                       
015900             MOVE +61          TO LAST-ADLAGOMR-UT                        
016000           WHEN +22 THRU +23                                              
016100             MOVE +61          TO LAST-ADLAGOMR-UT                        
016200           WHEN +30 THRU +33                                              
016300           WHEN +35 THRU +36                                              
016400             MOVE +60          TO LAST-ADLAGOMR-UT                        
016500           WHEN +34                                                       
016600           WHEN +37                                                       
016700             MOVE +64          TO LAST-ADLAGOMR-UT                        
016800           WHEN +28                                                       
016900           WHEN +45                                                       
017000           WHEN +39 THRU +43                                              
017100           WHEN +56 THRU +57                                              
017200           WHEN +72                                                       
017300           WHEN +77                                                       
017400           WHEN +78                                                       
017500             MOVE LAST-ADLAGOMR TO LAST-ADLAGOMR-UT                       
017600           WHEN OTHER                                                     
017700             MOVE +60          TO LAST-ADLAGOMR-UT                        
017800         END-EVALUATE                                                     
017900         MOVE W-ANT-ENHETSLAST TO LAST-KVANTAL-UT                         
018000         MOVE +0               TO LAST-KVBEART-UT                         
018100       ELSE                                                               
018200****   RADEN SPLITTRAS I TVA RADER                                        
018300         IF W-ANT-ENHETSLAST   > +0 AND                                   
018400            W-ENHETSLAST-REST  > +0                                       
018500             MOVE W-ENHETSLAST-REST TO LAST-KVBEART-UT                    
018600             MOVE W-ANT-ENHETSLAST  TO LAST-KVANTAL-UT                    
018700             EVALUATE LAST-ADLAGOMR                                       
018800               WHEN +10                                                   
018900                 MOVE LAST-ADLAGOMR TO LAST-ADLAGOMR-UT                   
019000               WHEN +11                                                   
019100                 MOVE +63      TO LAST-ADLAGOMR-UT                        
019200               WHEN +14 THRU +15                                          
019300                 MOVE +62      TO LAST-ADLAGOMR-UT                        
019400               WHEN +20                                                   
019500                 MOVE +61      TO LAST-ADLAGOMR-UT                        
019600               WHEN +22 THRU +23                                          
019700                 MOVE +61      TO LAST-ADLAGOMR-UT                        
019800               WHEN +30 THRU +33                                          
019900               WHEN +35 THRU +36                                          
020000                 MOVE +60      TO LAST-ADLAGOMR-UT                        
020100               WHEN +34                                                   
020200               WHEN +37                                                   
020300                 MOVE +64      TO LAST-ADLAGOMR-UT                        
020400               WHEN +28                                                   
020500               WHEN +45                                                   
020600               WHEN +39 THRU +43                                          
020700               WHEN +56 THRU +57                                          
020800               WHEN +72                                                   
020900               WHEN +77                                                   
021000               WHEN +78                                                   
021100                 MOVE LAST-ADLAGOMR TO LAST-ADLAGOMR-UT                   
021200               WHEN OTHER                                                 
021300                 MOVE +60      TO LAST-ADLAGOMR-UT                        
021400             END-EVALUATE                                                 
021500         END-IF                                                           
021600       END-IF                                                             
021700     END-IF                                                               
021800     .                                                                    
