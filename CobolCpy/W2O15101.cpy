000100 01  MOD-W2O15101.                                                        
000200*                                 MOD-COPYTEXT FÖR W2015100               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-KDPRODSL-IN      PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-KDPRODSL-UT      PIC X(2).                                    
001000*                                 PRODUKTSLAG                             
001100     03 MOD-IDPROJ-1         PIC X(4).                                    
001200*                                 PARTS PROJEKTIDENTITET                  
001300     03 MOD-IDPROJ-11        PIC X(4).                                    
001400*                                 PARTS PROJEKTIDENTITET                  
001500     03 MOD-IDPROJOBJ-1      PIC X(4).                                    
001600*                                 PROJEKTIDENTITET LV OBJEKT              
001700     03 MOD-IDPROJOBJ-11     PIC X(4).                                    
001800*                                 PROJEKTIDENTITET LV OBJEKT              
001900     03 MOD-IDPROJK-1        PIC X(4).                                    
002000*                                 PROJEKTIDENTITET KONSTRUKTION           
002100     03 MOD-IDPROJK-11       PIC X(4).                                    
002200*                                 PROJEKTIDENTITET KONSTRUKTION           
002300     03 MOD-RAD-PROJINFO     OCCURS 10 TIMES                              
002400                             INDEXED MOD-RAD-IND.                         
002500*                                 RADINFORMATION PER PROJEKT              
002600        05 MOD-RAD-IDPROJOBJ PIC X(4).                                    
002700*                                 PROJEKTIDENTITET LV OBJEKT              
002800        05 MOD-RAD-IDPROJ    PIC X(4).                                    
002900*                                 PARTS PROJEKTIDENTITET                  
003000        05 MOD-RAD-IDLKTO-ATTR                                            
003100                             PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300        05 MOD-RAD-IDLKTO    PIC Z(6)9.                                   
003400*                                 LAGERKONTO (FFHHHUU)                    
003500        05 MOD-RAD-RESLJUST-C1-ATTR                                       
003600                             PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800        05 MOD-RAD-RESLJUST-C1                                            
003900                             PIC 9.9.                                     
004000*                                 SÄKERHETSLAGER JUST C1                  
004100        05 MOD-RAD-RESLJUST-C2-ATTR                                       
004200                             PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400        05 MOD-RAD-RESLJUST-C2                                            
004500                             PIC 9.9.                                     
004600*                                 SÄKERHETSLAGER JUST C2                  
004700     03 MOD-IDPROJOBJ-IN-ATTR                                             
004800                             PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000     03 MOD-IDPROJOBJ-IN     PIC X(2).                                    
005100*                                 MFS BEHANDLING AV INPUTFÄLT             
005200     03 MOD-IDPROJ-IN-ATTR   PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400     03 MOD-IDPROJ-IN        PIC X(2).                                    
005500*                                 MFS BEHANDLING AV INPUTFÄLT             
005600     03 MOD-IDLKTO-IN-ATTR   PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800     03 MOD-IDLKTO-IN        PIC X(2).                                    
005900*                                 MFS BEHANDLING AV INPUTFÄLT             
006000     03 MOD-RESLJUST-C1-IN-ATTR                                           
006100                             PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300     03 MOD-RESLJUST-C1-IN   PIC X(2).                                    
006400*                                 MFS BEHANDLING AV INPUTFÄLT             
006500     03 MOD-RESLJUST-C2-IN-ATTR                                           
006600                             PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800     03 MOD-RESLJUST-C2-IN   PIC X(2).                                    
006900*                                 MFS BEHANDLING AV INPUTFÄLT             
007000     03 MOD-TEMFSINF         PIC X(61).                                   
007100*                                 INFORMATIONSMEDDELANDE                  
007200*** END COPY W2O15101    LENGTH=423                                       
