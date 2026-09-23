000100 01  MOD-W0O62301.                                                        
000200*                                 MOD TILL FRÅGA TRANSKÖN                 
000300*                                 DISPATCH TRANSAKTIONSDATABAS.           
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600*                                 SCREEN NUMBER                           
000700     03 MOD-TEMFSFEL         PIC X(40).                                   
000800*                                 MFS FELMEDDELANDE                       
000900*                                 MFS ERROR MESSAGE                       
001000     03 MOD-TIREGDAT-IN      PIC X(2).                                    
001100*                                 MFS BEHANDLING AV INPUTFÄLT             
001200*                                 MFS DISPOSITION OF INPUT FIELD          
001300     03 MOD-TIREGDAT-UT      PIC X(6).                                    
001400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001500*                                 REGISTRATION DATE (YYMMDD)              
001600     03 MOD-TIKLOCK-IN       PIC X(2).                                    
001700*                                 MFS BEHANDLING AV INPUTFÄLT             
001800*                                 MFS DISPOSITION OF INPUT FIELD          
001900     03 MOD-TIKLOCK-UT       PIC X(8).                                    
002000*                                 KLOCKSLAG (TTMMSSTH)                    
002100*                                 TIME OF DAY (HHMMSSTH)                  
002200     03 MOD-KDCMDVAL-ATTR    PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400     03 MOD-KDCMDVAL         PIC X(3).                                    
002500*                                 GENERELL KOMMANDOKOD                    
002600*                                 GENERAL COMMAND-CODE                    
002700     03 MOD-IDSNDNOD         PIC X(8).                                    
002800*                                 SÄNDANDE NODE IDENTITET                 
002900*                                 IDENTITY OF SENDING NODE                
003000     03 MOD-TIREGDAT         PIC X(6).                                    
003100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003200*                                 REGISTRATION DATE (YYMMDD)              
003300     03 MOD-TIKLOCK          PIC X(8).                                    
003400*                                 KLOCKSLAG (TTMMSSTH)                    
003500*                                 TIME OF DAY (HHMMSSTH)                  
003600     03 MOD-IDUSER           PIC X(8).                                    
003700*                                 ANVÄNDARENS SÄKERHETS ID                
003800*                                 USER SECURITY-IDENTITY                  
003900     03 MOD-KDKOMSTA         PIC X.                                       
004000*                                 KOMMUNIKATIONSSTATUS                    
004100*                                 COMMUNICATION STATUS                    
004200     03 MOD-IDSNDJOB         PIC X(8).                                    
004300*                                 SÄNDANDE JOB IDENTITET                  
004400*                                 IDENTITY OF SENDING JOB                 
004500     03 MOD-IDMFSMED         PIC X(3).                                    
004600*                                 MFS MEDDELANDE NUMMER                   
004700*                                 MFS MESSAGE NUMBER                      
004800     03 MOD-FELTEXT          PIC X(69).                                   
004900     03 MOD-TEMFSINF         PIC X(55).                                   
005000*                                 INFORMATIONSMEDDELANDE                  
005100*                                 INFORMATION MESSAGE                     
005200*** END OF VILMAII-COPY LENGTH= 233 BYTES                                 
