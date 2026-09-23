000100 01  W57073.                                                              
000200*                                 HÄNDELSER TILL ON-DEMAND                
000300     03 DAVERDAT             PIC 9(8).                                    
000400*                                 VERIFIKATIONSDATUM (ÅÅÅÅMMDD)           
000500     03 KDEKHHT              PIC X(3).                                    
000600*                                 EKONOMISK HUVUDHÄNDELSE                 
000700     03 KDEKSHT              PIC X(3).                                    
000800*                                 EKONOMISK SUBHÄNDELSE                   
000900     03 KDEKNIVA             PIC X(5).                                    
001000*                                 EKONOMISK HÄNDELSENIVÅ                  
001100     03 KDDOKTYP             PIC X(2).                                    
001200*                                 DOCUMENT TYPE                           
001300     03 IDDC                 PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 IDVERGL              PIC X(10).                                   
001600*                                 VERIFIKATIONSID FÖR HUVUDBOKEN          
001700     03 IDARTNR              PIC 9(9).                                    
001800*                                 ARTIKELNUMMER                           
001900     03 KDPRODSL             PIC 9(3).                                    
002000*                                 PRODUKTSLAG                             
002100     03 IDKONTO              PIC 9(10).                                   
002200*                                 KONTO                                   
002300     03 IDKST                PIC X(10).                                   
002400*                                 KOSTNADSSTÄLLE                          
002500     03 IDANALYS             PIC X(12).                                   
002600*                                 ANALYSNUMMER                            
002700     03 IDPRCTR              PIC X(10).                                   
002800*                                 PROFIT CENTER                           
002900     03 SUBEL                PIC 9(9)V9(2).                               
003000*                                 SUMMABELOPP                             
003100     03 IDTECKEN             PIC X.                                       
003200*                                 TECKEN                                  
003300     03 KDPOST               PIC X(2).                                    
003400*                                 POSTING KEY                             
003500     03 FLLSBOK              PIC X.                                       
003600*                                 LAGERAVBOKNING                          
003700     03 KVANTAL              PIC S9(7)           COMP-3.                  
003800*                                 ANTAL                                   
003900     03 PRARTSTD             PIC 9(7)V9(2).                               
004000*                                 ARTIKELSTANDARDPRIS                     
004100     03 KDTRADP              PIC X(4).                                    
004200*                                 TRADING PARTNER                         
004300*** END OF VILMAII-COPY LENGTH= 119 BYTES                                 
