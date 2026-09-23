000100 01  MOD-W0O62101.                                                        
000200*                                 MOD TILL FRÅGA/UPPDATERING AV           
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
003400     03 MOD-IDRADNR-IN       PIC X(2).                                    
003500*                                 MFS BEHANDLING AV INPUTFÄLT             
003600*                                 MFS DISPOSITION OF INPUT FIELD          
003700     03 MOD-IDRADNR-UT       PIC X(4).                                    
003800*                                 RADNUMMER                               
003900*                                 LINE NO                                 
004000     03 MOD-KDCMDVAL-ATTR    PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200     03 MOD-KDCMDVAL         PIC X(2).                                    
004300*                                 MFS BEHANDLING AV INPUTFÄLT             
004400*                                 MFS DISPOSITION OF INPUT FIELD          
004500     03 MOD-IDCPYTXT.                                                     
004600*                                 COPYTEXT IDENTITET                      
004700*                                 IDENTITY OF A COPYTEXT                  
004800        05 MOD-CT-IDSYSTEM   PIC X(4).                                    
004900*                                 VOLVO VCAS SYSTEMNUMMER                 
005000*                                 VOLVO VCAS SYSTEM NUMBER                
005100        05 MOD-CT-IDPTYP     PIC X(3).                                    
005200*                                 POSTTYP                                 
005300*                                 RECORD TYPE                             
005400        05 MOD-CT-IDVTYP     PIC X.                                       
005500*                                 POSTTYPSVERSION                         
005600*                                 RECORD TYPE VERSION                     
005700     03 MOD-IDLTERM          PIC X(8).                                    
005800*                                 LOGISKT TERMINALNAMN                    
005900*                                 IDENTITY OF LOGICAL TERMINAL            
006000     03 MOD-IDUSER           PIC X(8).                                    
006100*                                 ANVÄNDARENS SÄKERHETS ID                
006200*                                 USER SECURITY-IDENTITY                  
006300     03 MOD-IDMFSMED         PIC X(3).                                    
006400*                                 MFS MEDDELANDE NUMMER                   
006500*                                 MFS MESSAGE NUMBER                      
006600     03 MOD-KDKOMSTA         PIC X.                                       
006700*                                 KOMMUNIKATIONSSTATUS                    
006800*                                 COMMUNICATION STATUS                    
006900     03 MOD-FILLER           PIC X.                                       
007000     03 MOD-KDKOMBEH         PIC X.                                       
007100*                                 DISPATCHER FELHANTERING                 
007200*                                 DISPATCHER ERROR HANDLING               
007300     03 MOD-IDUSER-TRAN      PIC X(8).                                    
007400*                                 ANVÄNDARENS SÄKERHETS ID                
007500*                                 USER SECURITY-IDENTITY                  
007600     03 MOD-TRANSDATA-ATTR   PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800     03 MOD-TRANSDATA        PIC X(1200).                                 
007900     03 MOD-TEMFSINF         PIC X(55).                                   
008000*                                 INFORMATIONSMEDDELANDE                  
008100*                                 INFORMATION MESSAGE                     
008200*** END OF VILMAII-COPY LENGTH= 1387 BYTES                                
