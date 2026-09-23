000100 01  MOD-W0O62201.                                                        
000200*                                 MOD TILL FRÅGA TRANSKÖN                 
000300*                                 DISPATCH TRANSAKTIONSDATABAS.           
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600*                                 SCREEN NUMBER                           
000700     03 MOD-TEMFSFEL         PIC X(40).                                   
000800*                                 MFS FELMEDDELANDE                       
000900*                                 MFS ERROR MESSAGE                       
001000     03 MOD-IDSNDNOD-IN      PIC X(2).                                    
001100*                                 MFS BEHANDLING AV INPUTFÄLT             
001200*                                 MFS DISPOSITION OF INPUT FIELD          
001300     03 MOD-IDSNDNOD-UT      PIC X(8).                                    
001400*                                 SÄNDANDE NODE IDENTITET                 
001500*                                 IDENTITY OF SENDING NODE                
001600     03 MOD-IDSNDJOB-IN      PIC X(2).                                    
001700*                                 MFS BEHANDLING AV INPUTFÄLT             
001800*                                 MFS DISPOSITION OF INPUT FIELD          
001900     03 MOD-IDSNDJOB-UT      PIC X(8).                                    
002000*                                 SÄNDANDE JOB IDENTITET                  
002100*                                 IDENTITY OF SENDING JOB                 
002200     03 MOD-TIREGDAT-IN      PIC X(2).                                    
002300*                                 MFS BEHANDLING AV INPUTFÄLT             
002400*                                 MFS DISPOSITION OF INPUT FIELD          
002500     03 MOD-TIREGDAT-UT      PIC X(6).                                    
002600*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002700*                                 REGISTRATION DATE (YYMMDD)              
002800     03 MOD-TIKLOCK-IN       PIC X(2).                                    
002900*                                 MFS BEHANDLING AV INPUTFÄLT             
003000*                                 MFS DISPOSITION OF INPUT FIELD          
003100     03 MOD-TIKLOCK-UT       PIC X(8).                                    
003200*                                 KLOCKSLAG (TTMMSSTH)                    
003300*                                 TIME OF DAY (HHMMSSTH)                  
003400     03 MOD-KDKOMSTA-IN      PIC X(2).                                    
003500*                                 MFS BEHANDLING AV INPUTFÄLT             
003600*                                 MFS DISPOSITION OF INPUT FIELD          
003700     03 MOD-KDKOMSTA-UT      PIC X.                                       
003800*                                 KOMMUNIKATIONSSTATUS                    
003900*                                 COMMUNICATION STATUS                    
004000     03 MOD-RAD              OCCURS 14 TIMES.                             
004100        05 MOD-KDSVAR        PIC X.                                       
004200*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
004300*                                 RETURN CODE FROM PROGRAM                
004400        05 MOD-FILLER        PIC X.                                       
004500        05 MOD-IDSNDNOD      PIC X(8).                                    
004600*                                 SÄNDANDE NODE IDENTITET                 
004700*                                 IDENTITY OF SENDING NODE                
004800        05 MOD-FILLER        PIC X.                                       
004900        05 MOD-IDSNDJOB      PIC X(8).                                    
005000*                                 SÄNDANDE JOB IDENTITET                  
005100*                                 IDENTITY OF SENDING JOB                 
005200        05 MOD-FILLER        PIC X.                                       
005300        05 MOD-TIREGDAT      PIC 9(6).                                    
005400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
005500*                                 REGISTRATION DATE (YYMMDD)              
005600        05 MOD-FILLER        PIC X.                                       
005700        05 MOD-TIKLOCK       PIC 9(8).                                    
005800*                                 KLOCKSLAG (TTMMSSTH)                    
005900*                                 TIME OF DAY (HHMMSSTH)                  
006000        05 MOD-FILLER        PIC X.                                       
006100        05 MOD-IDCPYTXT.                                                  
006200*                                 COPYTEXT IDENTITET                      
006300*                                 IDENTITY OF A COPYTEXT                  
006400           07 MOD-CT-IDSYSTEM                                             
006500                             PIC X(4).                                    
006600*                                 VOLVO VCAS SYSTEMNUMMER                 
006700*                                 VOLVO VCAS SYSTEM NUMBER                
006800           07 MOD-CT-IDPTYP  PIC X(3).                                    
006900*                                 POSTTYP                                 
007000*                                 RECORD TYPE                             
007100           07 MOD-CT-IDVTYP  PIC X.                                       
007200*                                 POSTTYPSVERSION                         
007300*                                 RECORD TYPE VERSION                     
007400        05 MOD-FILLER        PIC X.                                       
007500        05 MOD-IDLTERM       PIC X(8).                                    
007600*                                 LOGISKT TERMINALNAMN                    
007700*                                 IDENTITY OF LOGICAL TERMINAL            
007800        05 MOD-FILLER        PIC X.                                       
007900        05 MOD-IDUSER        PIC X(8).                                    
008000*                                 ANVÄNDARENS SÄKERHETS ID                
008100*                                 USER SECURITY-IDENTITY                  
008200        05 MOD-FILLER        PIC X.                                       
008300        05 MOD-IDMFSMED      PIC X(3).                                    
008400*                                 MFS MEDDELANDE NUMMER                   
008500*                                 MFS MESSAGE NUMBER                      
008600        05 MOD-FILLER        PIC X.                                       
008700        05 MOD-KDKOMSTA      PIC X.                                       
008800*                                 KOMMUNIKATIONSSTATUS                    
008900*                                 COMMUNICATION STATUS                    
009000        05 MOD-FILLER        PIC X.                                       
009100        05 MOD-KDKOMBEH      PIC X.                                       
009200*                                 DISPATCHER FELHANTERING                 
009300*                                 DISPATCHER ERROR HANDLING               
009400     03 MOD-TEMFSINF         PIC X(55).                                   
009500*                                 INFORMATIONSMEDDELANDE                  
009600*                                 INFORMATION MESSAGE                     
009700*** END OF VILMAII-COPY LENGTH= 1120 BYTES                                
