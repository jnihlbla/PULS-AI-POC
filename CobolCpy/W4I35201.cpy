000100 01  MID-W4I35201.                                                        
000200*                                 MID-COPYTEXT FÖR BILD  4352             
000300*                                 ORDERKÖ I PRODUKTIONSKANAL              
000400     03 MID-IDPRC-IN.                                                     
000500*                                 PRODUKTIONSKANAL                        
000600        05 MID-IDPRCBAS      PIC X(3).                                    
000700*                                 PRC-BAS                                 
000800        05 MID-IDPRCVAR      PIC X.                                       
000900*                                 PRC-VARIANT                             
001000     03 MID-IDPRC-UT.                                                     
001100*                                 PRODUKTIONSKANAL                        
001200        05 MID-IDPRCBAS      PIC X(3).                                    
001300*                                 PRC-BAS                                 
001400        05 MID-IDPRCVAR      PIC X.                                       
001500*                                 PRC-VARIANT                             
001600     03 MID-IDUSER-IN        PIC X(8).                                    
001700*                                 ANVÄNDARENS SÄKERHETS ID                
001800     03 MID-IDUSER-UT        PIC X(8).                                    
001900*                                 ANVÄNDARENS SÄKERHETS ID                
002000     03 MID-IDBORD-IN        PIC X(3).                                    
002100*                                 PACK-BORD                               
002200     03 MID-IDBORD-UT        PIC X(3).                                    
002300*                                 PACK-BORD                               
002400     03 MID-IDTRP-IN.                                                     
002500*                                 TRANSPORTIDENTITET                      
002600        05 MID-IDTRPLOS      PIC X(3).                                    
002700*                                 TRANSPORTLÖSNING                        
002800        05 MID-IDTRPVAR      PIC X(2).                                    
002900*                                 TRANSPORTLÖSNINGSGRUPP                  
003000     03 MID-IDTRP-UT.                                                     
003100*                                 TRANSPORTIDENTITET                      
003200        05 MID-IDTRPLOS      PIC X(3).                                    
003300*                                 TRANSPORTLÖSNING                        
003400        05 MID-IDTRPVAR      PIC X(2).                                    
003500*                                 TRANSPORTLÖSNINGSGRUPP                  
003600     03 MID-IDDC-IN          PIC X(2).                                    
003700*                                 IDENTIFIERARE LAGER                     
003800     03 MID-IDDC-UT          PIC X(2).                                    
003900*                                 IDENTIFIERARE LAGER                     
004000     03 MID-KVORDER-ENTER    PIC 9(7).                                    
004100*                                 ANTAL ORDER                             
004200     03 MID-KVRADER-ENTER    PIC 9(5).                                    
004300*                                 ANTAL RADER                             
004400     03 MID-IDTRP-ENTER.                                                  
004500*                                 TRANSPORTIDENTITET                      
004600        05 MID-IDTRPLOS      PIC X(3).                                    
004700*                                 TRANSPORTLÖSNING                        
004800        05 MID-IDTRPVAR      PIC X(2).                                    
004900*                                 TRANSPORTLÖSNINGSGRUPP                  
005000     03 MID-TIAAMMDD-ENTER   PIC 9(6).                                    
005100*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
005200     03 MID-TIHHMM-ENTER     PIC X(5).                                    
005300*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
005400     03 MID-TIRFS-ENTER      PIC 9(10).                                   
005500*                                 KLART FÖR TRANSPORT ÅÅMMDDTTMM          
005600     03 MID-TIUTSKR-ENTER    PIC 9(6).                                    
005700*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
005800     03 MID-KVORDER-NEXT     PIC 9(7).                                    
005900*                                 ANTAL ORDER                             
006000     03 MID-KVRADER-NEXT     PIC 9(5).                                    
006100*                                 ANTAL RADER                             
006200     03 MID-IDPLKLST-NEXT    PIC 9(3).                                    
006300*                                 PLOCKLISTNUMMER                         
006400     03 MID-IDPLKLST-ENTER   PIC 9(3).                                    
006500*                                 PLOCKLISTNUMMER                         
006600     03 MID-IDPRC-SPAR.                                                   
006700*                                 PRODUKTIONSKANAL                        
006800        05 MID-IDPRCBAS      PIC X(3).                                    
006900*                                 PRC-BAS                                 
007000        05 MID-IDPRCVAR      PIC X.                                       
007100*                                 PRC-VARIANT                             
007200     03 MID-TILST-O-ENTER    PIC 9(10).                                   
007300*                                 SENASTE STARTTIDPUNKT FÖR ORDER         
007400     03 MID-TABELLRAD        OCCURS 11 TIMES.                             
007500*                                 GRUPP MED TABELL RADER                  
007600        05 MID-FLORDDEL      PIC X.                                       
007700*                                 ALLMÄN FLAGGA                           
007800     03 MID-ORDERDELSNYCKEL  OCCURS 12 TIMES.                             
007900*                                 GRUPP MED NYCKEL TILL ORDERDEL          
008000        05 MID-IDPRODNR-KEY  PIC X(7).                                    
008100*                                 PRODUKTIONSNUMMER                       
008200        05 MID-IDPLKLST-KEY  PIC X(3).                                    
008300*                                 PLOCKLISTNUMMER                         
008400     03 MID-TIUTSTID-ENTER   PIC 9(6).                                    
008500*                                 UTSKRIFTSTID (TTMMSS)                   
008600     03 MID-IDTRP-NEXT.                                                   
008700*                                 TRANSPORTIDENTITET                      
008800        05 MID-IDTRPLOS      PIC X(3).                                    
008900*                                 TRANSPORTLÖSNING                        
009000        05 MID-IDTRPVAR      PIC X(2).                                    
009100*                                 TRANSPORTLÖSNINGSGRUPP                  
009200     03 MID-TIAAMMDD-NEXT    PIC 9(6).                                    
009300*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
009400     03 MID-TIHHMM-NEXT      PIC X(5).                                    
009500*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
009600     03 MID-TIRFS-NEXT       PIC 9(10).                                   
009700*                                 KLART FÖR TRANSPORT ÅÅMMDDTTMM          
009800     03 MID-TILST-O-NEXT     PIC 9(10).                                   
009900*                                 SENASTE STARTTIDPUNKT FÖR ORDER         
010000     03 MID-TIUTSKR-NEXT     PIC 9(6).                                    
010100*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
010200     03 MID-TIUTSTID-NEXT    PIC 9(6).                                    
010300*                                 UTSKRIFTSTID (TTMMSS)                   
010400     03 MID-FLKLAR           PIC X.                                       
010500*                                 AVSLUTNINGSMARKERING                    
010600     03 MID-KDPRT-PU         PIC X(3).                                    
010700*                                 PRINTERKOD PACKUNDERLAG                 
010800     03 MID-KDPRT-PLE        PIC X(3).                                    
010900*                                 PRINTERKOD PLOCKETIKETTER               
011000*** END OF VILMAII-COPY LENGTH= 312 BYTES                                 
