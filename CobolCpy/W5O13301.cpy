000100 01  MOD-W5O13301.                                                        
000200*                                 MODCOPYTEXT TILL W50133.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MOD-IDDC             PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MOD-RELANDCO-EXP     PIC Z(2)9.9.                                 
001200*                                 LANDING COST PROCENT EXPORTRANS         
001300     03 MOD-RELANDCO-EXP-IN-ATTR                                          
001400                             PIC X(2).                                    
001500*                                 MFS ATTRIBUTFÄLT                        
001600     03 MOD-RELANDCO-EXP-IN  PIC Z(2)9.9.                                 
001700*                                 LANDING COST PROCENT EXPORTRANS         
001800     03 MOD-RELANDCO-TO      PIC Z(2)9.9.                                 
001900*                                 LANDING COST PROCENT EXPORTRANS         
002000     03 MOD-TILANDCO         PIC 9(6).                                    
002100*                                 STARTDATUM LANDING COST FAKTOR          
002200     03 MOD-TILANDCO-IN-ATTR PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400     03 MOD-TILANDCO-IN      PIC 9(6).                                    
002500*                                 STARTDATUM LANDING COST FAKTOR          
002600     03 MOD-RELANDCO-FROM    PIC Z(2)9.9.                                 
002700*                                 LANDING COST PROCENT EXPORTRANS         
002800     03 MOD-RELANDCO-FROM-IN-ATTR                                         
002900                             PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100     03 MOD-RELANDCO-FROM-IN PIC Z(2)9.9.                                 
003200*                                 LANDING COST PROCENT EXPORTRANS         
003300     03 MOD-REDMTRL          PIC Z(2)9.                                   
003400*                                 DIREKT MATERIAL PROCENT-PÅSLAG          
003500     03 MOD-REDMTRL-IN-ATTR  PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700     03 MOD-REDMTRL-IN       PIC Z(2)9.                                   
003800*                                 DIREKT MATERIAL PROCENT-PÅSLAG          
003900     03 MOD-REDIRLON         PIC Z(2)9.                                   
004000*                                 DIREKT LÖN PROCENT-PÅSLAG               
004100     03 MOD-REDIRLON-IN-ATTR PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300     03 MOD-REDIRLON-IN      PIC Z(2)9.                                   
004400*                                 DIREKT LÖN PROCENT-PÅSLAG               
004500     03 MOD-IDUSER           PIC X(8).                                    
004600*                                 ANVÄNDARENS SÄKERHETS ID                
004700     03 MOD-TIUPPDAT         PIC 9(6).                                    
004800*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
004900     03 MOD-AREA             OCCURS 45 TIMES.                             
005000        05 MOD-IDDISTR       PIC Z(4).                                    
005100*                                 DISTRIKTNUMMER                          
005200     03 MOD-TEMFSINF         PIC X(55).                                   
005300*                                 INFORMATIONSMEDDELANDE                  
005400*** END OF VILMAII-COPY LENGTH= 356 BYTES                                 
