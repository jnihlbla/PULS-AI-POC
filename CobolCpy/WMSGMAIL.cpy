000100 01  MAIL-WMSGMAIL.                                                       
000200*                                 SÄNDNING AV EPOST/MEMO FRÅN MPP         
000300*                                 NEDANSTÅENDE COPYTEXT SKICKAS           
000400*                                 TILL TRANS W0T541X VIA ALT-PCB.         
000500*                                 EXEMPEL:                                
000600*                                 CALL CBLTDLI USING ISRT                 
000700*                                              ALT-PCB                    
000800*                                              MAIL-WMSGMAIL              
000900*                                                                         
001000*                                 ANTAL RADER KVMEMRAD MÅSTE VARA         
001100*                                 IFYLLT. FULLSTÄNDIGT EPOST-ID           
001200*                                 INKLUSIVE SNABEL-A OCH DOMÄN            
001300*                                 MÅSTE ANGES.                            
001400     03 MAIL-KVLL            PIC S9(4)           COMP                     
001500                             VALUE +8813.                                 
001600*                                 LÄNGD PÅ DENNA TRANSAKTION              
001700*                                 LENGTH OF THIS TRANSACTION              
001800     03 MAIL-KDZ1            PIC X                                        
001900                             VALUE LOW-VALUE.                             
002000*                                 POS 3 I LRECL I MID/MOD                 
002100*                                 POS 3 IN LRECL IN MID/MOD               
002200     03 MAIL-KDZ2            PIC X                                        
002300                             VALUE LOW-VALUE.                             
002400*                                 POS 4 I LRECL I MID/MOD                 
002500*                                 POS 4 IN LRECL IN MID/MOD               
002600     03 MAIL-KDTRANS         PIC X(8)                                     
002700                             VALUE 'W0T541X '.                            
002800*                                 TRANSAKTION W0T541X                     
002900*                                 TRANSACTION W0T541X                     
003000     03 MAIL-IDTRANS         PIC X(4)                                     
003100                             VALUE SPACES.                                
003200*                                 BILDNUMMER                              
003300*                                 SCREEN NUMBER                           
003400     03 MAIL-KDMFSFOR        PIC X                                        
003500                             VALUE SPACE.                                 
003600*                                 TYP AV MFS-FORMAT                       
003700*                                 1 = W-FORMAT  2 = N-FORMAT              
003800*                                 TYPE OF MFS FORMAT                      
003900     03 MAIL-IDMAIL          PIC X(60)                                    
004000                             VALUE SPACES.                                
004100*                                 MAIL ADRESS                             
004200*                                 MAIL ADDRESS                            
004300     03 MAIL-IDMAILTTL       PIC X(250)                                   
004400                             VALUE SPACES.                                
004500*                                 E-MAIL TITEL                            
004600*                                 E-MAIL TITLE                            
004700     03 MAIL-IDMAIL-SENDER   PIC X(60)                                    
004800                             VALUE SPACES.                                
004900*                                 AVSÄNDANDE MAIL ID/PWD                  
005000*                                 ID/PWD OF SENDER                        
005100     03 MAIL-IDPFDEF         PIC X(8)                                     
005200                             VALUE SPACES.                                
005300*                                 IBM PSF FORMSDEF,PAGEDEF                
005400*                                 IBM PSF FORMSDEF,PAGEDEF                
005500     03 MAIL-FLCARRCNTL      PIC X                                        
005600                             VALUE SPACE.                                 
005700*                                 INGÅR STYRTECKEN I DATA?                
005800*                                 IS CARRRIAGE CONTROL INCLUDED?          
005900     03 MAIL-KVMAILLN        PIC 9(2)                                     
006000                             VALUE ZEROS.                                 
006100*                                 ANTAL RADER I ETT MAIL                  
006200*                                 NUMBER OF LINES IN A MAIL               
006300     03 MAIL-TEMAIL          OCCURS 99 TIMES                              
006400                             PIC X(85)                                    
006500                             VALUE SPACES.                                
006600*                                 TEXTRAD MAIL                            
006700*                                 MAIL TEXT LINE                          
006800*** END OF VILMAII-COPY LENGTH= 8813 BYTES                                
