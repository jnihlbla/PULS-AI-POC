000100 01  W4O53201.                                                            
000200*                                                                         
000300     03 TRANS-NUMMER.                                                     
000400*                                 TRANSAKTIONS-NUMMER                     
000500*                                                                         
000600        05 TRANS-SIFF-1      PIC X.                                       
000700        05 TRANS-SIFF-2      PIC X.                                       
000800        05 TRANS-SIFF-3      PIC X.                                       
000900        05 TRANS-SIFF-4      PIC X.                                       
001000     03 MESSAGE-RAD1         PIC X(40).                                   
001100*                                 MEDDELANDEFÄLT PÅ RAD 1                 
001200     03 IDDISTR-IN-ATTR      PIC X(2).                                    
001300*                                 MFS ATTRIBUTFÄLT                        
001400     03 IDDISTR-IN           PIC X(2).                                    
001500*                                 DISTRIKTNUMMER      IDDISTR-008         
001600     03 IDDISTR-UT           PIC X(4).                                    
001700*                                 DISTRIKTNUMMER                          
001800     03 IDKUNDNR-IN-ATTR     PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000     03 IDKUNDNR-IN          PIC X(2).                                    
002100*                                 MFS BEHANDLING AV INPUTFÄLT             
002200     03 IDKUNDNR-UT          PIC X(6).                                    
002300*                                 KUNDNUMMER                              
002400     03 KDFRAKT-IN-ATTR      PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600     03 KDFRAKT-IN           PIC X(2).                                    
002700*                                 FRAKTSÄTT C1-C2 TILL KUND               
002800     03 KDFRAKT-UT           PIC X(2).                                    
002900*                                 FRAKTSÄTT C1-C2 TILL KUND               
003000     03 IDSKEPPN-IN-ATTR     PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200     03 IDSKEPPN-IN          PIC X(2).                                    
003300*                                 MFS BEHANDLING AV INPUTFÄLT             
003400     03 IDSKEPPN-UT          PIC X(7).                                    
003500*                                 SKEPPNINGSNUMMER                        
003600     03 IDDC-IN-ATTR         PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800     03 IDDC-IN              PIC X(2).                                    
003900*                                 MFS BEHANDLING AV INPUTFÄLT             
004000     03 IDDC-UT              PIC X(2).                                    
004100*                                 IDENTIFIERARE LAGER                     
004200     03 R4533-35-FAELT.                                                   
004300*                                 VÄRDEN FÖR KOMMUNIKATION MED            
004400*                                 W40533 OCH W40535                       
004500        05 R4533-35-TESTFAELT                                             
004600                             PIC X(4).                                    
004700        05 FILLER            PIC X(201).                                  
004800     03 SPARADE-VAERDEN.                                                  
004900        05 SPARAD-NYCKEL     PIC X(31).                                   
005000        05 SPARAD-RDE5-FILLER REDEFINES SPARAD-NYCKEL.                    
005100           07 SPARAD-RDE5.                                                
005200              09 SPARAT-IDKUNDNR-5                                        
005300                             PIC X(7).                                    
005400*                                 KUNDNUMMER                              
005500              09 SPARAT-IDKUNDNR-5-NUM REDEFINES SPARAT-IDKUNDNR-5        
005600                             PIC 9(7).                                    
005700*                                 KUNDNUMMER                              
005800              09 SPARAT-KDORDSTA-5                                        
005900                             PIC X(2).                                    
006000              09 SPARAT-KDORDSTA-5-NUM REDEFINES SPARAT-KDORDSTA-5        
006100                             PIC 9(2).                                    
006200              09 SPARAT-IDPRODNR-5                                        
006300                             PIC X(6).                                    
006400*                                 PRODUKTIONSNUMMER                       
006500              09 SPARAT-IDPRODNR-5-NUM REDEFINES SPARAT-IDPRODNR-5        
006600                             PIC 9(6).                                    
006700*                                 PRODUKTIONSNUMMER                       
006800              09 SPARAT-IDKUNDRF-5                                        
006900                             PIC X(10).                                   
007000*                                 KUNDENS REFERENS (ORDERID)              
007100              09 SPARAT-IDPLKLST-5                                        
007200                             PIC X(3).                                    
007300*                                 PLOCKLISTNUMMER                         
007400              09 SPARAT-IDPLKLST-5-NUM REDEFINES SPARAT-IDPLKLST-5        
007500                             PIC 9(3).                                    
007600*                                 PLOCKLISTNUMMER                         
007700           07 FILLER         PIC X(3).                                    
007800        05 SPARAD-WDE7 REDEFINES SPARAD-NYCKEL.                           
007900           07 SPARAT-IDKUNDNR-7                                           
008000                             PIC X(7).                                    
008100*                                 KUNDNUMMER                              
008200           07 SPARAT-IDKUNDNR-7-NUM REDEFINES SPARAT-IDKUNDNR-7           
008300                             PIC 9(7).                                    
008400*                                 KUNDNUMMER                              
008500           07 SPARAT-IDPRODNR-7                                           
008600                             PIC X(6).                                    
008700*                                 PRODUKTIONSNUMMER                       
008800           07 SPARAT-IDPRODNR-7-NUM REDEFINES SPARAT-IDPRODNR-7           
008900                             PIC 9(6).                                    
009000*                                 PRODUKTIONSNUMMER                       
009100           07 SPARAT-IDKOLLI-7                                            
009200                             PIC X(5).                                    
009300*                                 KOLLINUMMER                             
009400           07 SPARAT-IDKOLLI-7-NUM REDEFINES SPARAT-IDKOLLI-7             
009500                             PIC 9(5).                                    
009600*                                 KOLLINUMMER                             
009700           07 SPARAT-IDKUNDRF-7                                           
009800                             PIC X(10).                                   
009900*                                 KUNDENS REFERENS (ORDERID)              
010000           07 SPARAT-IDPLKLST-7                                           
010100                             PIC X(3).                                    
010200*                                 PLOCKLISTNUMMER                         
010300           07 SPARAT-IDPLKLST-7-NUM REDEFINES SPARAT-IDPLKLST-7           
010400                             PIC 9(3).                                    
010500*                                 PLOCKLISTNUMMER                         
010600     03 AREA.                                                             
010700        05 RADER             OCCURS 13 TIMES                              
010800                             INDEXED RAD-IX.                              
010900           07 KOLUMNER       OCCURS 3 TIMES                               
011000                             INDEXED KOL-IX.                              
011100              09 ORDER-KUNDREF                                            
011200                             PIC X(19).                                   
011300     03 MESSAGE-RAD23        PIC X(79).                                   
011400*                                 MEDDELANDEFÄLT PÅ RAD 23                
