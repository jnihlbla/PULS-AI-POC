000100 01  4126-WDGX4126.                                                       
000200*                                 ANSVARIG TABELL FÖR UTSKRIFT            
000300*                                 AV KREDITNOTOR                          
000400*                                 VÄRDE INTERVALL                         
000500*                                 FYSISK NYCKEL: KEY4126                  
000600*                                 (SUKRENOT-FOM + SUKRENOT-TOM)           
000700*                                                                         
000800     03 4126-SUKRENOT-FOM    PIC S9(7)           COMP-3.                  
000900*                                 KREDITNOTASUMMA FOM                     
001000*                                 CREDIT NOTE TOTAL  FROM                 
001100     03 4126-SUKRENOT-TOM    PIC S9(7)           COMP-3.                  
001200*                                 KREDITNOTASUMMA TOM                     
001300*                                 CREDIT NOTE TOTAL UP TO                 
001400     03 4126-BEANST          PIC X(25).                                   
001500*                                 ANSTÄLLDS NAMN                          
001600*                                 NAME OF EMPLOYED                        
001700     03 4126-FLKREPRT        PIC X.                                       
001800*                                 UTSKRIFTSFLAGGA KREDITNOTA              
001900     03 4126-IDUSER-ADM      PIC X(8).                                    
002000*                                 ANVÄNDAR-ID ADMINISTRATIV KONTR         
002100*                                 USER ID ADMINISTRATIVE INSPEC.          
002200     03 4126-IDUSER-ATTUPD   PIC X(8).                                    
002300*                                 ANVÄNDAR-ID ATTESTRÄTT K-NOTOR          
002400*                                 USER ID ATTEST RIGHT CREDIT NOT         
002500*                                 ES                                      
002600     03 4126-TIUPPDAT        PIC S9(7)           COMP-3.                  
002700*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
002800*                                 UPDATING DATE     (YYMMDD)              
002900*** END OF VILMAII-COPY LENGTH= 54 BYTES                                  
