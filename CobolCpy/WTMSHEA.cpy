000010*** EDIT ALLOWED                                                          
000100 01  WTMSHEA.                                                             
000200*                                 TMS PACKAGING HEADER RECORD             
000220*                                                                         
000230     03 TMS-IDPTYP            PIC X(03)   VALUE 'HEA'.                    
000240*                                                                         
000250     03 TMS-IDLOPNR           PIC 9(04).                                  
000260*                                                                         
000501     03 FILLER-1              PIC X(03)   VALUE 'NEW'.                    
000502*                                                                         
000503     03 TMS-IDBOKN            PIC X(16).                                  
000504*                                                                         
000505     03 TMS-DATUM-ISSUED      PIC X(10).                                  
000506*                                                                         
000507     03 TMS-DATUM-BOOKED      PIC X(10).                                  
000508*                                                                         
000509     03 TMS-IDLEVNR-GSDB-SEND PIC X(05).                                  
000510*                                                                         
000520     03 TMS-IDLEVNR-GSDB-REC  PIC X(05).                                  
000530*                                                                         
000560*** END OF VILMAII-COPY LENGTH=56                                         
