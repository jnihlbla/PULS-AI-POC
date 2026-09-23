000100 01  W57070.                                                              
000200*                                 POSTER FÖR AVSTÄMN. O ANALYS            
000300     03 DAREGDAT             PIC 9(8).                                    
000400*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
000500     03 TIKLOCK              PIC S9(9)           COMP-3.                  
000600*                                 KLOCKSLAG (TTMMSSTH)                    
000700     03 KDEKHHT              PIC X(3).                                    
000800*                                 EKONOMISK HUVUDHÄNDELSE                 
000900     03 KDEKSHT              PIC X(3).                                    
001000*                                 EKONOMISK SUBHÄNDELSE                   
001100     03 KDEKNIVA             PIC X(5).                                    
001200*                                 EKONOMISK HÄNDELSENIVÅ                  
001300     03 IDDC                 PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 IDVERGL              PIC X(10).                                   
001600*                                 VERIFIKATIONSID FÖR HUVUDBOKEN          
001700     03 IDARTNR              PIC S9(9)           COMP-3.                  
001800*                                 ARTIKELNUMMER                           
001900     03 KDPRODSL             PIC S9(3)           COMP-3.                  
002000*                                 PRODUKTSLAG                             
002100     03 IDKONTO              PIC 9(10).                                   
002200*                                 KONTO                                   
002300     03 SUBEL                PIC S9(11)V9(2)     COMP-3.                  
002400*                                 SUMMABELOPP           SUBEL-002         
002500     03 FLLSBOK              PIC X.                                       
002600*                                 LAGERAVBOKNING                          
002700     03 KVANTAL              PIC S9(7)           COMP-3.                  
002800*                                 ANTAL                                   
002900     03 PRAVCOST             PIC S9(7)V9(2)      COMP-3.                  
003000*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
003100     03 DAVERDAT             PIC 9(8).                                    
003200*                                 VERIFIKATIONSDATUM (ÅÅÅÅMMDD)           
003300*** END OF VILMAII-COPY LENGTH= 78 BYTES                                  
