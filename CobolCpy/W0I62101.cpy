000100 01  MID-W0I62101.                                                        
000200*                                 MID TILL FRÅGA/UPPDATERING AV           
000300*                                 DISPATCH TRANSAKTIONSDATABAS.           
000400     03 MID-IDSNDNOD         PIC X(8).                                    
000500*                                 SÄNDANDE NODE IDENTITET                 
000600*                                 IDENTITY OF SENDING NODE                
000700     03 MID-IDSNDJOB         PIC X(8).                                    
000800*                                 SÄNDANDE JOB IDENTITET                  
000900*                                 IDENTITY OF SENDING JOB                 
001000     03 MID-TIREGDAT         PIC X(6).                                    
001100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001200*                                 REGISTRATION DATE (YYMMDD)              
001300     03 MID-TIKLOCK          PIC X(8).                                    
001400*                                 KLOCKSLAG (TTMMSSTH)                    
001500*                                 TIME OF DAY (HHMMSSTH)                  
001600     03 MID-IDRADNR-IN       PIC X(4).                                    
001700*                                 RADNUMMER                               
001800*                                 LINE NO                                 
001900     03 MID-IDRADNR-UT       PIC X(4).                                    
002000*                                 RADNUMMER                               
002100*                                 LINE NO                                 
002200     03 MID-KDCMDVAL         PIC X(3).                                    
002300*                                 GENERELL KOMMANDOKOD                    
002400*                                 GENERAL COMMAND-CODE                    
002500     03 MID-TRANSDATA        PIC X(1200).                                 
002600*** END OF VILMAII-COPY LENGTH= 1241 BYTES                                
