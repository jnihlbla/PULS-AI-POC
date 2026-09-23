000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300                                                                          
000400 PROGRAM-ID.     W510KONT.                                                
000500 AUTHOR.        KARL JOHAN HANSSON.                                       
000600 DATE-WRITTEN.  OKTOBER 1994.                                             
000700*    REMARKS.                                                             
000800*                                                                         
000900*        ANROP:          SKER GENOM CALL W510KONT                         
001000*                                                                         
001100*        BEHANDLING      DENNA MODUL HÄMTAR KONTO OCH                     
001200*                        EV. ANALYSNUMMER                                 
001300*                        FÖR KONTERING AV FAKTURORS OCH                   
001400*                        KREDITNOTORS TILLÄGGSKOSTNADER I                 
001500*                        EKONOMISYSTEMEN.                                 
001600*                        PROGRAMMET ANROPAR W510MARK FÖR ATT HÄMTA        
001700*                        MARKNADSKOD SOM LEDER TILL RÄTT ANALYS-          
001800*                        NUMMER I PROGRAMMETS ANALYSNRTABELL.             
001900*                                                                         
002000*        LÄNKAREA        W510KONT                                         
002100*                                                                         
002200*      SKICKA MED KDCALL OCH IDDISTR                                      
002300*                                                                         
002400*      MÖJLIGA VÄRDEN PÅ KDCALL SE COPYTEXTEN W510KONT.                   
002500*                                                                         
002600*      SOM SVAR FÅS ALLTID KONTO OCH ANALYSNUMMER.                        
002700*                  OM EJ TRÄFF ÄR DESSA NOLL/BLANKA                       
002800     EJECT                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000 DATA DIVISION.                                                           
003100     SKIP2                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400*    -- CHECKED BY WY2000                                                 
003500 77  IDPGM              PIC X(8)     VALUE 'W510KONT'.                    
003600 77  OK-SW                       PIC X VALUE 'N'.                         
003700     88 OK                             VALUE 'J'.                         
003800     88 NOT-OK                         VALUE 'N'.                         
003900 01  WS-IDANALYS                 PIC 9(12) VALUE ZERO.                    
004000 01  WS-KDMARK                   PIC 9(2)  VALUE ZERO.                    
004100 01  TABELL-IDKTO.                                                        
004200     03 IDKTO-AVDRAG         PIC 9(10)  VALUE 302108.                     
004300     03 IDKTO-LEG            PIC 9(10)  VALUE 572002.                     
004400     03 IDKTO-FORS           PIC 9(10)  VALUE 571414.                     
004500     03 IDKTO-FRAKT          PIC 9(10)  VALUE 579989.                     
004600                                                                          
004700 01  FILLER                      PIC X(16) VALUE 'ANALYSTABELL'.          
004800 01  ANALYSTABELL.                                                        
004900     03  ANALYSNRVARDEN.                                                  
005000       05  FILLER   PIC X(15) VALUE '01 158600000435'.                    
005100       05  FILLER   PIC X(15) VALUE '02 158600000435'.                    
005200       05  FILLER   PIC X(15) VALUE '03 158600000527'.                    
005300       05  FILLER   PIC X(15) VALUE '04 158600000528'.                    
005400       05  FILLER   PIC X(15) VALUE '05 158600000435'.                    
005500       05  FILLER   PIC X(15) VALUE '06 158600000502'.                    
005600       05  FILLER   PIC X(15) VALUE '07 158600000450'.                    
005700       05  FILLER   PIC X(15) VALUE '08 158600000451'.                    
005800       05  FILLER   PIC X(15) VALUE '09 158600000452'.                    
005900       05  FILLER   PIC X(15) VALUE '10 158600000449'.                    
006000       05  FILLER   PIC X(15) VALUE '11 158600000502'.                    
006100       05  FILLER   PIC X(15) VALUE '12 158600000449'.                    
006200       05  FILLER   PIC X(15) VALUE '13 158600000459'.                    
006300       05  FILLER   PIC X(15) VALUE '14 158600000651'.                    
006400       05  FILLER   PIC X(15) VALUE '15 158600000459'.                    
006500       05  FILLER   PIC X(15) VALUE '16 158600000502'.                    
006600       05  FILLER   PIC X(15) VALUE '17 158600000471'.                    
006700       05  FILLER   PIC X(15) VALUE '18 158600000461'.                    
006800       05  FILLER   PIC X(15) VALUE '19 158600000527'.                    
006900       05  FILLER   PIC X(15) VALUE '20 158600000460'.                    
007000       05  FILLER   PIC X(15) VALUE '21 158600000460'.                    
007100       05  FILLER   PIC X(15) VALUE '22 158600000467'.                    
007200       05  FILLER   PIC X(15) VALUE '23 158600000472'.                    
007300       05  FILLER   PIC X(15) VALUE '24 158600000513'.                    
007400       05  FILLER   PIC X(15) VALUE '25 158600000656'.                    
007500       05  FILLER   PIC X(15) VALUE '26 158600000473'.                    
007600       05  FILLER   PIC X(15) VALUE '27 158600000474'.                    
007700       05  FILLER   PIC X(15) VALUE '28 158600000475'.                    
007800       05  FILLER   PIC X(15) VALUE '29 158600000514'.                    
007900       05  FILLER   PIC X(15) VALUE '30 158600000458'.                    
008000       05  FILLER   PIC X(15) VALUE '31 158600000462'.                    
008100       05  FILLER   PIC X(15) VALUE '32 158600000476'.                    
008200       05  FILLER   PIC X(15) VALUE '33 158600000473'.                    
008300       05  FILLER   PIC X(15) VALUE '34 158600000506'.                    
008400       05  FILLER   PIC X(15) VALUE '35 158600000463'.                    
008500       05  FILLER   PIC X(15) VALUE '36 158600000465'.                    
008600       05  FILLER   PIC X(15) VALUE '37 158600000464'.                    
008700       05  FILLER   PIC X(15) VALUE '38 158600000511'.                    
008800       05  FILLER   PIC X(15) VALUE '39 158600000466'.                    
008900       05  FILLER   PIC X(15) VALUE '40 158600000516'.                    
009000       05  FILLER   PIC X(15) VALUE '41 158600000508'.                    
009100       05  FILLER   PIC X(15) VALUE '42 158600000512'.                    
009200       05  FILLER   PIC X(15) VALUE '43 158600000488'.                    
009300       05  FILLER   PIC X(15) VALUE '44 158600000468'.                    
009400       05  FILLER   PIC X(15) VALUE '45 158600000526'.                    
009500       05  FILLER   PIC X(15) VALUE '46 158600000491'.                    
009600       05  FILLER   PIC X(15) VALUE '47 158600000516'.                    
009700       05  FILLER   PIC X(15) VALUE '48 158600000652'.                    
009800       05  FILLER   PIC X(15) VALUE '49 158600000526'.                    
009900       05  FILLER   PIC X(15) VALUE '50 158600000469'.                    
010000       05  FILLER   PIC X(15) VALUE '51 158600000528'.                    
010100       05  FILLER   PIC X(15) VALUE '52 158600000528'.                    
010200       05  FILLER   PIC X(15) VALUE '53 158600000524'.                    
010300       05  FILLER   PIC X(15) VALUE '54 158600000526'.                    
010400       05  FILLER   PIC X(15) VALUE '55 158600000528'.                    
010500       05  FILLER   PIC X(15) VALUE '56 158600000526'.                    
010600       05  FILLER   PIC X(15) VALUE '57 158600000526'.                    
010700       05  FILLER   PIC X(15) VALUE '58 158600000528'.                    
010800       05  FILLER   PIC X(15) VALUE '59 158600000523'.                    
010900       05  FILLER   PIC X(15) VALUE '60 158600000526'.                    
011000       05  FILLER   PIC X(15) VALUE '61 158600000526'.                    
011100       05  FILLER   PIC X(15) VALUE '62 158600000657'.                    
011200       05  FILLER   PIC X(15) VALUE '63 158600000516'.                    
011300       05  FILLER   PIC X(15) VALUE '64 158600000526'.                    
011400       05  FILLER   PIC X(15) VALUE '65 158600001828'.                    
011500       05  FILLER   PIC X(15) VALUE '66 158600000522'.                    
011600       05  FILLER   PIC X(15) VALUE '67 158600000526'.                    
011700       05  FILLER   PIC X(15) VALUE '68 158600000914'.                    
011800       05  FILLER   PIC X(15) VALUE '69 158600000516'.                    
011900       05  FILLER   PIC X(15) VALUE '70 158600000523'.                    
012000       05  FILLER   PIC X(15) VALUE '71 158600000518'.                    
012100       05  FILLER   PIC X(15) VALUE '72 158600000519'.                    
012200       05  FILLER   PIC X(15) VALUE '73 158600000515'.                    
012300       05  FILLER   PIC X(15) VALUE '74 158600000502'.                    
012400       05  FILLER   PIC X(15) VALUE '75 158600002604'.                    
012500       05  FILLER   PIC X(15) VALUE '76 158600000520'.                    
012600       05  FILLER   PIC X(15) VALUE '77 158600000521'.                    
012700       05  FILLER   PIC X(15) VALUE '78 158600000527'.                    
012800       05  FILLER   PIC X(15) VALUE '79 158600000496'.                    
012900       05  FILLER   PIC X(15) VALUE '80 158600000502'.                    
013000       05  FILLER   PIC X(15) VALUE '81 158600000497'.                    
013100       05  FILLER   PIC X(15) VALUE '82 158600000517'.                    
013110       05  FILLER   PIC X(15) VALUE '83 158600000650'.                    
013300       05  FILLER   PIC X(15) VALUE '84 158600001302'.                    
013400       05  FILLER   PIC X(15) VALUE '85 158600000501'.                    
013500       05  FILLER   PIC X(15) VALUE '86 158600000502'.                    
013600       05  FILLER   PIC X(15) VALUE '87 158600000502'.                    
013700       05  FILLER   PIC X(15) VALUE '88 158600000499'.                    
013800       05  FILLER   PIC X(15) VALUE '89 158600000525'.                    
013900       05  FILLER   PIC X(15) VALUE '90 158600000495'.                    
014000       05  FILLER   PIC X(15) VALUE '91 158600000500'.                    
014100       05  FILLER   PIC X(15) VALUE '92 158600000498'.                    
014200       05  FILLER   PIC X(15) VALUE '93 158600000502'.                    
014300       05  FILLER   PIC X(15) VALUE '94 158600000494'.                    
014400       05  FILLER   PIC X(15) VALUE '95 158600000516'.                    
014500                                                                          
014600     03 ANALYS REDEFINES ANALYSNRVARDEN                                   
014700        OCCURS  95 INDEXED BY IX.                                         
014800       05  MARKKOD     PIC 9(2).                                          
014900       05  FILLER      PIC X.                                             
015000       05  ANALYSNUM   PIC 9(12).                                         
015100                                                                          
015200     EJECT                                                                
015300 01  GENERELLA-SUBPROGRAM.                                                
015400     03  W510MARK                    PIC X(8)  VALUE 'W510MARK'.          
015500                                                                          
015600 01  FILLER                          PIC X(16) VALUE 'W510MARK '.         
015700*                                                                         
015800*01  -COPY W510MARK                                                       
015900     EJECT                                                                
016000 LINKAGE SECTION.                                                         
016100     SKIP2                                                                
016200*    -COPY W510KONT                                                       
016300     EJECT                                                                
016400 PROCEDURE DIVISION USING KONT-W510KONT.                                  
016600                                                                          
016700 STYR SECTION.                                                            
016800                                                                          
016810     MOVE SPACE                         TO KONT-IDANALYS                  
016900                                           KONT-IDKST                     
017000     MOVE ZERO                          TO KONT-IDKONTO                   
017200     PERFORM B-HAMTA-IDANALYS                                             
017300                                                                          
017400     EVALUATE KONT-KDCALL                                                 
017500        WHEN 3   MOVE IDKTO-LEG         TO KONT-IDKONTO                   
017600                 MOVE WS-IDANALYS       TO KONT-IDANALYS                  
017700                                                                          
017800        WHEN 4   MOVE IDKTO-FRAKT       TO KONT-IDKONTO                   
017900                 MOVE WS-IDANALYS       TO KONT-IDANALYS                  
018000                                                                          
018100        WHEN 5   MOVE IDKTO-FORS        TO KONT-IDKONTO                   
018200                 MOVE WS-IDANALYS       TO KONT-IDANALYS                  
018300                                                                          
018400        WHEN 6   MOVE IDKTO-AVDRAG      TO KONT-IDKONTO                   
018500                 MOVE SPACE             TO KONT-IDANALYS                  
018600     END-EVALUATE                                                         
018700                                                                          
018800     MOVE ZERO TO RETURN-CODE                                             
018900     GOBACK                                                               
019000     .                                                                    
019100     SKIP3                                                                
019200 B-HAMTA-IDANALYS SECTION.                                                
019300                                                                          
019400* HÄR ANROPAS W510MARK FÖR ATT HÄMTA MARKNAD, FÖR VARJE                   
019500* MARKNADSKOD FRÅN 1 TILL 95 FINNS ETT ANALYSNUMMER                       
019600* DETTA ANALYSNUMMER LÄGGS I KONT-IDANALYS                                
019700                                                                          
019800     MOVE +0                          TO MARK-KDCALL                      
019900     MOVE +0                          TO WS-KDMARK                        
020000     MOVE KONT-IDDISTR                TO MARK-IDDISTR                     
020100     CALL W510MARK USING MARK-W510MARK                                    
020200     MOVE MARK-KDMARK-BUDG            TO WS-KDMARK                        
020300                                                                          
020400     SET IX TO 1                                                          
020500     SEARCH ANALYS                                                        
020600       AT END MOVE ZERO               TO WS-IDANALYS                      
020700       WHEN WS-KDMARK = MARKKOD(IX)                                       
020800         MOVE ANALYSNUM(IX)           TO WS-IDANALYS                      
020900     END-SEARCH                                                           
021000                                                                          
021100     .                                                                    
