000100 01  RHI-W903RHI.                                                         
000200*                                 NYUPPL ORDER VDI                        
000300*                                 POSTTYP = RHI                           
000400     03 RHI-KVRADER          PIC 9(5).                                    
000500*                                 ANTAL RADER                             
000600     03 RHI-HUVUD.                                                        
000700*                                 ORDERHUVUD VDI                          
000800        05 RHI-KDORDKL       PIC 9.                                       
000900*                                 ORDERKLASS                              
001000        05 RHI-KDFRAKT       PIC X(2).                                    
001100*                                 FRAKTSÄTT DC TILL KUND                  
001200        05 RHI-FLRESTN       PIC X.                                       
001300*                                 RESTNOTERING ?                          
001400        05 RHI-BEKUNDRF      PIC X(10).                                   
001500*                                 KUNDENS REFERENS                        
001600        05 RHI-BEVARREF      PIC X(10).                                   
001700*                                 VÅR REFERENS                            
001800        05 RHI-KDTPOTYP      PIC X.                                       
001900*                                 TYP AV TIDPLANERAD ORDER                
002000        05 RHI-IDKAMPRF      PIC X(7).                                    
002100*                                 KAMPANJREFERENS                         
002200     03 RHI-RADER            OCCURS 128 TIMES.                            
002300*                                 ORDERRADER VDI                          
002400        05 RHI-IDARTNR       PIC 9(9).                                    
002500*                                 ARTIKELNUMMER                           
002600        05 RHI-KVBEART       PIC 9(6).                                    
002700*                                 BESTÄLLT ANTAL STYCKEN                  
002800        05 RHI-FLSLATT       PIC X.                                       
002900*                                 FLAGGA SOM ANGER OM KVSLATT SKA         
003000*                                 LL BERÄKNAS ELLER EJ                    
003100*                                 OM FLRESTN = J OCH FLSLATT = J,         
003200*                                  DÅ BERÄKNAS KVSLATT                    
003300        05 RHI-KDKVBRYT      PIC X.                                       
003400*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
003500        05 RHI-BERADREF      PIC X(10).                                   
003600*                                 KUNDENS RADREFERENS                     
003700        05 RHI-KDDSP         PIC X.                                       
003800*                                 PÅVERKAN PÅ DSP                         
003900*** END OF VILMAII-COPY LENGTH= 3621 BYTES                                
