000100 01  MID-W0I62301.                                                        
000200*                                 MID TILL TRANSKÖN                       
000300*                                 DISPATCH TRANSAKTIONSDATABAS.           
000400     03 MID-TIREGDAT-IN      PIC X(6).                                    
000500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
000600*                                 REGISTRATION DATE (YYMMDD)              
000700     03 MID-TIREGDAT-UT      PIC X(6).                                    
000800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
000900*                                 REGISTRATION DATE (YYMMDD)              
001000     03 MID-TIKLOCK-IN       PIC X(8).                                    
001100*                                 KLOCKSLAG (TTMMSSTH)                    
001200*                                 TIME OF DAY (HHMMSSTH)                  
001300     03 MID-TIKLOCK-UT       PIC X(8).                                    
001400*                                 KLOCKSLAG (TTMMSSTH)                    
001500*                                 TIME OF DAY (HHMMSSTH)                  
001600     03 MID-KDCMDVAL         PIC X(3).                                    
001700*                                 GENERELL KOMMANDOKOD                    
001800*                                 GENERAL COMMAND-CODE                    
001900     03 MID-IDSNDNOD         PIC X(8).                                    
002000*                                 SÄNDANDE NODE IDENTITET                 
002100*                                 IDENTITY OF SENDING NODE                
002200     03 MID-TIREGDAT         PIC X(6).                                    
002300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002400*                                 REGISTRATION DATE (YYMMDD)              
002500     03 MID-TIKLOCK          PIC X(8).                                    
002600*                                 KLOCKSLAG (TTMMSSTH)                    
002700*                                 TIME OF DAY (HHMMSSTH)                  
002800     03 MID-IDUSER           PIC X(8).                                    
002900*                                 ANVÄNDARENS SÄKERHETS ID                
003000*                                 USER SECURITY-IDENTITY                  
003100     03 MID-KDKOMSTA         PIC X.                                       
003200*                                 KOMMUNIKATIONSSTATUS                    
003300*                                 COMMUNICATION STATUS                    
003400     03 MID-IDSNDJOB         PIC X(8).                                    
003500*                                 SÄNDANDE JOB IDENTITET                  
003600*                                 IDENTITY OF SENDING JOB                 
003700*** END OF VILMAII-COPY LENGTH= 70 BYTES                                  
