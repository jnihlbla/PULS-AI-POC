000100 01  W513562.                                                             
000200*                                 COPYTEXT TILL ADJUSTMENTS > 500         
000300*                                 0 SEK LDC                               
000400     03 IDAFPRCD             PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 IDDC                 PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 TIAAVV               PIC 9(4).                                    
000900*                                 ÅR - VECKA  (ÅÅVV)                      
001000     03 IDARTNR              PIC Z(7)9.                                   
001100*                                 ARTIKELNUMMER                           
001200     03 BEART                PIC X(25).                                   
001300*                                 ARTIKELBENÄMNING                        
001400     03 TIINVDAT             PIC 9(5).                                    
001500*                                 INVENTERINGSDATUM                       
001600     03 ADLAGOMR             PIC 9(2).                                    
001700*                                 LAGEROMRÅDE                             
001800     03 KVJUSTKV             PIC -(7)9.                                   
001900*                                 JUSTERAD KVANTITET                      
002000     03 KDPRODSL             PIC Z9.                                      
002100*                                 PRODUKTSLAG                             
002200*** END OF VILMAII-COPY LENGTH= 66 BYTES                                  
