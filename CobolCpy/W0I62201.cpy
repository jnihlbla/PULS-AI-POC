000100 01  MID-W0I62201.                                                        
000200*                                 MID TILL TRANSKÖN                       
000300*                                 DISPATCH TRANSAKTIONSDATABAS.           
000400     03 MID-IDSNDNOD-IN      PIC X(8).                                    
000500*                                 SÄNDANDE NODE IDENTITET                 
000600*                                 IDENTITY OF SENDING NODE                
000700     03 MID-IDSNDJOB-IN      PIC X(8).                                    
000800*                                 SÄNDANDE JOB IDENTITET                  
000900*                                 IDENTITY OF SENDING JOB                 
001000     03 MID-TIREGDAT-IN      PIC X(6).                                    
001100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001200*                                 REGISTRATION DATE (YYMMDD)              
001300     03 MID-TIKLOCK-IN       PIC X(8).                                    
001400*                                 KLOCKSLAG (TTMMSSTH)                    
001500*                                 TIME OF DAY (HHMMSSTH)                  
001600     03 MID-KDKOMSTA-IN      PIC X.                                       
001700*                                 KOMMUNIKATIONSSTATUS                    
001800*                                 COMMUNICATION STATUS                    
001900     03 MID-RAD              OCCURS 14 TIMES.                             
002000        05 MID-KDSVAR        PIC X.                                       
002100*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
002200*                                 RETURN CODE FROM PROGRAM                
002300        05 MID-FILLER        PIC X.                                       
002400        05 MID-IDSNDNOD      PIC X(8).                                    
002500*                                 SÄNDANDE NODE IDENTITET                 
002600*                                 IDENTITY OF SENDING NODE                
002700        05 MID-FILLER        PIC X.                                       
002800        05 MID-IDSNDJOB      PIC X(8).                                    
002900*                                 SÄNDANDE JOB IDENTITET                  
003000*                                 IDENTITY OF SENDING JOB                 
003100        05 MID-FILLER        PIC X.                                       
003200        05 MID-TIREGDAT      PIC X(6).                                    
003300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003400*                                 REGISTRATION DATE (YYMMDD)              
003500        05 MID-FILLER        PIC X.                                       
003600        05 MID-TIKLOCK       PIC X(8).                                    
003700*                                 KLOCKSLAG (TTMMSSTH)                    
003800*                                 TIME OF DAY (HHMMSSTH)                  
003900*** END OF VILMAII-COPY LENGTH= 521 BYTES                                 
