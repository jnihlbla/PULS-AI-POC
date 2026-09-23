000100 01  MAIL-WZ01MAIL.                                                       
000200*                                 SÄNDNING AV EPOST FRÅN MPP/BMP.         
000300*                                 NEDANSTÅENDE COPYTEXT SKICKAS           
000400*                                 VIA WZ01-DISPATCHERN TILL               
000500*                                 CARPARTS.PULS.SENDMAIL                  
000600*                                 OCH ETT ALT-PCB SOM HETER               
000700*                                 SENDMAIL OCH PEKAR PÅ W0T541X           
000800*                                 MÅSTE DÅ FINNAS I PSB:ET                
000900*                                                                         
001000*                                 ANTAL RADER KVMEMRAD MÅSTE VARA         
001100*                                 IFYLLT. FULLSTÄNDIGT EPOST-ID           
001200*                                 INKLUSIVE SNABEL-A OCH DOMÄN            
001300*                                 MÅSTE ANGES.                            
001400     03 MAIL-IDTRANS         PIC X(4)                                     
001500                             VALUE SPACES.                                
001600*                                 BILDNUMMER                              
001700*                                 SCREEN NUMBER                           
001800     03 MAIL-KDMFSFOR        PIC X                                        
001900                             VALUE SPACE.                                 
002000*                                 TYP AV MFS-FORMAT                       
002100*                                 1 = W-FORMAT  2 = N-FORMAT              
002200*                                 TYPE OF MFS FORMAT                      
002300     03 MAIL-IDMAIL          PIC X(60)                                    
002400                             VALUE SPACES.                                
002500*                                 MAIL ADRESS                             
002600*                                 MAIL ADDRESS                            
002700     03 MAIL-IDMAILTTL       PIC X(250)                                   
002800                             VALUE SPACES.                                
002900*                                 E-MAIL TITEL                            
003000*                                 E-MAIL TITLE                            
003100     03 MAIL-IDMAIL-SENDER   PIC X(60)                                    
003200                             VALUE SPACES.                                
003300*                                 AVSÄNDANDE MAIL ID/PWD                  
003400*                                 ID/PWD OF SENDER                        
003500     03 MAIL-IDPFDEF         PIC X(8)                                     
003600                             VALUE SPACES.                                
003700*                                 IBM PSF FORMSDEF,PAGEDEF                
003800*                                 IBM PSF FORMSDEF,PAGEDEF                
003900     03 MAIL-FLCARRCNTL      PIC X                                        
004000                             VALUE SPACE.                                 
004100*                                 INGÅR STYRTECKEN I DATA?                
004200*                                 IS CARRRIAGE CONTROL INCLUDED?          
004300     03 MAIL-KVMAILLN        PIC 9(2)                                     
004400                             VALUE ZEROS.                                 
004500*                                 ANTAL RADER I ETT MAIL                  
004600*                                 NUMBER OF LINES IN A MAIL               
004700     03 MAIL-TEMAIL          OCCURS 99 TIMES                              
004800                             PIC X(85)                                    
004900                             VALUE SPACES.                                
005000*                                 TEXTRAD MAIL                            
005100*                                 MAIL TEXT LINE                          
005200*** END OF VILMAII-COPY LENGTH= 8801 BYTES                                
