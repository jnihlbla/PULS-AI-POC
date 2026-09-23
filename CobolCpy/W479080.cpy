000100 01  W479080-CTX.                                                         
000200     03 W479080-001-GRP.                                                  
000300        05 IDPTYP            PIC X(3).                                    
000400*                                 POSTTYP                                 
000500        05 KDWRTYP           PIC 9(4).                                    
000600*                                 TYP                                     
000700        05 IDDC              PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900        05 IDDISTR           PIC S9(5)           COMP-3.                  
001000*                                 DISTRIKTNUMMER                          
001100        05 KDPRODSL          PIC S9(3)           COMP-3.                  
001200*                                 PRODUKTSLAG                             
001300        05 TIAAVV            PIC S9(5)           COMP-3.                  
001400*                                 ÅR - VECKA  (ÅÅVV)                      
001500     03 KDMARK               PIC S9(3)           COMP-3.                  
001600*                                 MARKNADSKOD                             
001700     03 SUARTNTO             PIC S9(9)V9(2)      COMP-3.                  
001800*                                 SUMMA RADVÄRDE TILL NETTOPRIS           
001900*** END COPY W479080     LENGTH=25                                        
