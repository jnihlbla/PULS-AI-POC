000100 01  MID-W4I55101.                                                        
000200*                                 MID-COPYTEXT PGM W40551                 
000300*                                 FRÅGA PÅ TRANSPORTÖR                    
000400     03 MID-IDDC-IN          PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600     03 MID-IDDC-UT          PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 MID-IDTRP-IN.                                                     
000900*                                 TRANSPORTIDENTITET                      
001000        05 MID-IDTRPLOS      PIC X(3).                                    
001100*                                 TRANSPORTLÖSNING                        
001200        05 MID-IDTRPVAR      PIC X(2).                                    
001300*                                 TRANSPORTLÖSNINGSGRUPP                  
001400     03 MID-IDTRP-UT.                                                     
001500*                                 TRANSPORTIDENTITET                      
001600        05 MID-IDTRPLOS      PIC X(3).                                    
001700*                                 TRANSPORTLÖSNING                        
001800        05 MID-IDTRPVAR      PIC X(2).                                    
001900*                                 TRANSPORTLÖSNINGSGRUPP                  
002000     03 MID-KDODELSTA-IN     PIC X.                                       
002100*                                 ORDERDELSTATUS                          
002200     03 MID-KDODELSTA-UT     PIC X.                                       
002300*                                 ORDERDELSTATUS                          
002400     03 MID-TIAAMMDD-ENTER   PIC 9(6).                                    
002500*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
002600     03 MID-TIHHMM-ENTER     PIC 9(4).                                    
002700*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
002800     03 MID-IDORDER-ENTER    PIC 9(7).                                    
002900*                                 VOLVO PARTS ORDERNUMMER                 
003000     03 MID-TIAAMMDD-NEXT    PIC 9(6).                                    
003100*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
003200     03 MID-TIHHMM-NEXT      PIC 9(4).                                    
003300*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
003400     03 MID-IDORDER-NEXT     PIC 9(7).                                    
003500*                                 VOLVO PARTS ORDERNUMMER                 
003600     03 MID-FLAGGA-EOF       PIC X.                                       
003700*                                 JA/NEJ-FLAGGA                           
003800     03 MID-FLAGGA-SCROLL    PIC X.                                       
003900*                                 JA/NEJ-FLAGGA                           
004000     03 MID-VLORDNTO-SPAR    PIC 9(4)V9(3).                               
004100*                                 ORDERVOLYM NETTO (M3)                   
004200     03 MID-VKORDNTO-SPAR    PIC 9(6)V9(1).                               
004300*                                 ORDERVIKT NETTO (KG)                    
004400     03 MID-SUORDV-SPAR      PIC 9(9)V9(2).                               
004500*                                 SUMMA ORDERVÄRDE                        
