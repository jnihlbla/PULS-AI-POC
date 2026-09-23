000100*                                                                         
000200    05 POST.                                                              
000300       10 POSTTYP                    PIC X(2).                            
000400       10 DATA-AREA                  PIC X(118).                          
010800       10 POSTTYP21-AREA             REDEFINES DATA-AREA.                 
000500          15 FILAVSANDARNAMN         PIC X(30).                           
000500          15 FILAVSANDARORGNR        PIC X(17).                           
000500          15 FILDATUM                PIC X(8).                            
000500          15 FILTID                  PIC X(4).                            
000500          15 FILMOTTAGARNAMN         PIC X(30).                           
000500          15 FILMOTTAGARORGNR        PIC X(17).                           
000500          15 FILMOTTAGARID           PIC X(8).                            
000500          15 FILLER                  PIC X(4).                            
010800       10 POSTTYP22-AREA             REDEFINES DATA-AREA.                 
000500          15 ATGARDSKOD              PIC X(1).                            
000500          15 ARTIKELNR               PIC X(14).                           
000500          15 STATNR                  PIC X(14).                           
000500          15 LEVID                   PIC X(9).                            
000500          15 ARTIKELURSPRUNG         PIC X(2).                            
000500          15 BLANDATURSPRUNG         PIC X(1).                            
000500          15 BENAMNING-SE            PIC X(25).                           
000500          15 PARTS-PREFIX-ARTIKELNR  PIC X(3).                            
000500          15 PARTS-ARTIKELNR         PIC X(47).                           
000500          15 FILLER                  PIC X(2).                            
013000*                                                                         
013100*** END COPY ARTSTAUR    LENGTH=120  OLD LENGTH=80                        
