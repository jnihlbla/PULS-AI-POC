000010 01  MID-W6I19501.                                                        
000020*                                 MIDCOPYTEXT TILL W60195.                
000030     03 MID-IDPRTLST         PIC X(8).                                    
000040*                                 LOGISK PRINTER+LISTA IDENTITET          
000050     03 MID-IDPGM            PIC X(8).                                    
000060*                                 PROGRAM IDENTITET                       
000070     03 MID-KVPOST           PIC 9(7).                                    
000080*                                 RÄKNARE, ANTAL POSTER                   
000090     03 MID-FLAGG-POST       OCCURS 15 TIMES.                             
000100        05 MID-IDLOPNRM      PIC 9(9).                                    
000110*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
000120*                                 (0VVDLLLLK)                             
000130        05 MID-IDRADNR       PIC 9(4).                                    
000140*                                 RADNUMMER                               
000150        05 MID-IDARTNR       PIC 9(8).                                    
000160*                                 ARTIKELNUMMER                           
000170        05 MID-KVINLART      PIC 9(6).                                    
000180*                                 ANTAL I PARTIRAD                        
000190        05 MID-BEART         PIC X(25).                                   
000200*                                 ARTIKELBENÄMNING                        
000210        05 MID-ADLAGOMR      PIC 9(2).                                    
000220*                                 LAGEROMRÅDE                             
000230        05 MID-ADGANG        PIC 9(2).                                    
000240*                                 GÅNG                                    
000250        05 MID-ADPLATS       PIC 9(5).                                    
000260*                                 LAGERPLATSNUMMER                        
      *** END COPY W6I19501    LENGTH=938                                       
