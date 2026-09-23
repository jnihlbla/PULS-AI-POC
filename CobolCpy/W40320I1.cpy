000100 01  REQU-W40320I1.                                                       
000200*                                 REQUEST TO PGM W40320                   
000300     03 REQU-KDCALL          PIC 9(3).                                    
000400*                                 ANROPSTYP                               
000500     03 REQU-IDDC            PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 REQU-IDANSTNR        PIC 9(5).                                    
000800*                                 ANSTÄLLNINGSNUMMER                      
000900     03 REQU-IDQUEUENR       PIC 9(3).                                    
001000*                                 PRE PRINT QUEUE NUMBER                  
001100     03 REQU-IDPRCPLK        PIC X(4).                                    
001200*                                 ID FÖR EN PLOCKRUNDA                    
001300     03 REQU-IDLOTNR-PLK     PIC 9(3).                                    
001400*                                 VAGN-NUMMER FÖR PLOCKRUNDA              
001500*** END OF VILMAII-COPY LENGTH= 20 BYTES                                  
