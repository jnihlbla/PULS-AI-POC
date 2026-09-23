000100 01  REQU-W60361I1.                                                       
000200*                                 REQU-COPYTEXT FÖR W6036100              
000300*                                                                         
000400     03 REQU-IDDC            PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 REQU-OUTPUT          OCCURS 500 TIMES.                            
000800*                                 OCCURS CLAUSE FOR W603611I COPY         
000900*                                 TEXT                                    
001000        05 REQU-IDARTNR      PIC 9(8).                                    
001100*                                 ARTIKELNUMMER                           
001200*                                 PART NUMBER                             
001300        05 REQU-KVANTAL      PIC 9(7).                                    
001400*                                 ANTAL                                   
001500*                                 NUMBER                                  
001600        05 REQU-KVQPACK      PIC 9(7).                                    
001700*                                 ANTAL                                   
001800*                                 NUMBER                                  
001900*** END OF VILMAII-COPY LENGTH= 11002 BYTES                               
