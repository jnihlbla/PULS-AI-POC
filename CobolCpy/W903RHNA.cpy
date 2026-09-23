000100 01  RHN-W903RHN.                                                         
000200*                                 NYUPPL ORDER VDI                        
000300*                                 MED ADDRESS TILLÄGG                     
000400*                                 POSTTYP = RHN                           
000500     03 RHN-KVRADER          PIC 9(5).                                    
000600*                                 ANTAL RADER                             
000700     03 RHN-HUVUD.                                                        
000800*                                 ORDERHUVUD VDI                          
000900        05 RHN-KDORDKL       PIC 9.                                       
001000*                                 ORDERKLASS                              
001100        05 RHN-KDFRAKT       PIC X(2).                                    
001200*                                 FRAKTSÄTT DC TILL KUND                  
001300        05 RHN-FLRESTN       PIC X.                                       
001400*                                 RESTNOTERING ?                          
001500        05 RHN-BEKUNDRF      PIC X(10).                                   
001600*                                 KUNDENS REFERENS                        
001700        05 RHN-BEVARREF      PIC X(10).                                   
001800*                                 VÅR REFERENS                            
001900        05 RHN-KDTPOTYP      PIC X.                                       
002000*                                 TYP AV TIDPLANERAD ORDER                
002100        05 RHN-IDKAMPRF      PIC X(7).                                    
002200*                                 KAMPANJREFERENS                         
002300     03 RHN-BEGMT.                                                        
002400*                                 GODSMOTTAGARNAMN                        
002500        05 RHN-BEGMT-RAD1    PIC X(35).                                   
002600*                                 GODSMOTTAGARNAMN RAD 1                  
002700        05 RHN-BEGMT-RAD2    PIC X(35).                                   
002800*                                 GODSMOTTAGARNAMN RAD 2                  
002900     03 RHN-ADGMT-GATA       PIC X(35).                                   
003000*                                 GODSMOTTAGARADRESS GATA                 
003100     03 RHN-ADGMT-PADR       PIC X(35).                                   
003200*                                 GODSMOTTAGARADRESS POSTADRESS           
003300     03 RHN-BELAGINS-DEL     PIC X(60).                                   
003400*                                 DEL AV LAGERINSTRUKTION                 
003500     03 RHN-RADER            OCCURS 100 TIMES.                            
003600*                                 ORDERRADER VDI                          
003700        05 RHN-IDARTNR       PIC 9(9).                                    
003800*                                 ARTIKELNUMMER                           
003900        05 RHN-KVBEART       PIC 9(6).                                    
004000*                                 BESTÄLLT ANTAL STYCKEN                  
004100        05 RHN-FLSLATT       PIC X.                                       
004200*                                 FLAGGA SOM ANGER OM KVSLATT SKA         
004300*                                 LL BERÄKNAS ELLER EJ                    
004400*                                 OM FLRESTN = J OCH FLSLATT = J,         
004500*                                  DÅ BERÄKNAS KVSLATT                    
004600        05 RHN-KDKVBRYT      PIC X.                                       
004700*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
004800        05 RHN-BERADREF      PIC X(10).                                   
004900*                                 KUNDENS RADREFERENS                     
005000        05 RHN-KDDSP         PIC X.                                       
005100*                                 PÅVERKAN PÅ DSP                         
005200*** END OF VILMAII-COPY LENGTH= 3037 BYTES                                
