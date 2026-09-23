000100 01  MID-W3I17701.                                                        
000200*                                 MID-COPYTEXT FÖR BILD 3177              
000300*                                 EXCHANGEPRM RE0                         
000400     03 MID-IDARTNR          PIC X(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 MID-IDDISTR          PIC X(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800     03 MID-IDKUNDNR         PIC X(6).                                    
000900*                                 KUNDNUMMER                              
001000     03 MID-IDBYTRAP         PIC X(7).                                    
001100*                                 RAPPORTNUMMER  BYTES                    
001200     03 MID-KDBYTSTA         PIC X.                                       
001300*                                 STATUSKOD BYTESOBJEKT                   
001400     03 MID-IDDC             PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 MID-XPRM-RE0-GRP     OCCURS 13 TIMES.                             
001700*                                 XCHANGE HISTORY RETURNS                 
001800        05 MID-KDSVAR        PIC X.                                       
001900*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
002000*** END OF VILMAII-COPY LENGTH= 42 BYTES                                  
