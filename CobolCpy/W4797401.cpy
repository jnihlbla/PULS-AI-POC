000100 01  W4797401.                                                            
000200*                                 PACKUNDERLAG OCH OBKR HISTORIK          
000300*                                 KATALOG-IDENTITET                       
000400     03 IDDISTR              PIC S9(5)           COMP-3.                  
000500*                                 DISTRIKTNUMMER                          
000600     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000700*                                 KUNDNUMMER                              
000800     03 IDKUNDRF             PIC X(10).                                   
000900*                                 KUNDENS REFERENS (ORDERID)              
001000     03 IDDC                 PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 IDSEGM               PIC X(6).                                    
001300*                                 SEGMENT                                 
001400     03 TIFAKT               PIC S9(7)           COMP-3.                  
001500*                                 FAKTURERINGSDATUM (≈≈MMDD)              
001600     03 TIHIST-OBKR REDEFINES TIFAKT                                      
001700                             PIC S9(7)           COMP-3.                  
001800*                                 FLYTTNINGSDATUM                         
001900*** END COPY W4797401    LENGTH=29                                        
