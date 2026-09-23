000010 01  MID-W6I21301.                                                        
000020*                                 COPYTEXT FÖR MID W6I21301               
000030*                                                                         
000040     03 MID-IDARTNR-IN       PIC X(9).                                    
000050*                                 ARTIKELNUMMER                           
000060     03 MID-IDARTNR-UT       PIC X(9).                                    
000070*                                 ARTIKELNUMMER                           
000080     03 MID-IDLEVNR-IN       PIC X(5).                                    
000090*                                 LEVERANTÖRNUMMER                        
000100     03 MID-IDLEVNR-UT       PIC X(5).                                    
000110*                                 LEVERANTÖRNUMMER                        
000120     03 MID-IDKVAINFI        PIC X(2).                                    
000130*                                 RADNR FÖR KVALITETSKONTROLLTEXT         
000140     03 MID-IDKVAINFU        PIC X(2).                                    
000150*                                 RADNR FÖR KVALITETSKONTROLLTEXT         
000160     03 MID-TIREGDAT-IN      PIC X(6).                                    
000170*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
000180     03 MID-TIREGDAT-UT      PIC X(6).                                    
000190*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
000200     03 MID-KDKVAINF-IN      PIC X.                                       
000210*                                 TYP AV KVAL.INFO FÖR ARTIKEL            
000220     03 MID-KDKVAINF-UT      PIC X.                                       
000230*                                 TYP AV KVAL.INFO FÖR ARTIKEL            
000240     03 MID-IDKVAINF-NEXT    PIC X(2).                                    
000250*                                 RADNR FÖR KVALITETSKONTROLLTEXT         
000260     03 MID-IDKVAINF-ENTER   PIC X(2).                                    
000270*                                 RADNR FÖR KVALITETSKONTROLLTEXT         
000280     03 MID-INPUT.                                                        
000290*                                                                         
000300        05 MID-FLKVASAK      PIC X.                                       
000310*                                 FLAGGA KVALITETSSÄKRAD                  
000320        05 MID-FLUPG         PIC X.                                       
000330*                                 FLAGGA UTFALLSPROV GODKÄNT              
000340        05 MID-KDCMD-LEV     PIC X.                                       
000350        05 MID-IDKVAINF-IN   PIC 9(2).                                    
000360*                                 RADNR FÖR KVALITETSKONTROLLTEXT         
000370        05 MID-IDPROVPL-PRI-IN                                            
000380                             PIC X.                                       
000390*                                 PROVTAGNINGSPLAN PRIM.KONTROLL          
000400        05 MID-KVSKPLOT-PRI-IN                                            
000410                             PIC 9.                                       
000420        05 MID-BEANST-IN     PIC X(25).                                   
000430*                                 ANSTÄLLDS NAMN                          
000440        05 MID-IDTFN-IN      PIC X(20).                                   
000450*                                 TELEFONNUMMER EXTERNT                   
000460        05 MID-KDCMD-SPEC    PIC X.                                       
      *** END COPY W6I21301    LENGTH=103                                       
