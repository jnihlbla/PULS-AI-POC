000100 01  MID-W5I30901.                                                        
000200*                                 MID-COPYTEXT FÖR BILD                   
000300*                                 INVENTERINGSKÖ 2                        
000400     03 MID-IDDC-IN          PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600     03 MID-IDDC-UT          PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 MID-ADLAGOMR-IN      PIC X(2).                                    
000900*                                 LAGEROMRÅDE                             
001000     03 MID-ADLAGOMR-UT      PIC X(2).                                    
001100*                                 LAGEROMRÅDE                             
001200     03 MID-ADGANG-IN        PIC X(2).                                    
001300*                                 GÅNG                                    
001400     03 MID-ADGANG-UT        PIC X(2).                                    
001500*                                 GÅNG                                    
001600     03 MID-ADPLATS-IN       PIC X(5).                                    
001700*                                 LAGERPLATSNUMMER                        
001800     03 MID-ADPLATS-UT       PIC X(5).                                    
001900*                                 LAGERPLATSNUMMER                        
002000     03 MID-IDPRTOMG-IN      PIC 9.                                       
002100*                                 PRINT OMGÅNG FÖR AUT.JUSTERING          
002200     03 MID-IDPRTOMG-UT      PIC 9.                                       
002300*                                 PRINT OMGÅNG FÖR AUT.JUSTERING          
002400     03 MID-FLINVSKR-IN      PIC X.                                       
002500*                                 INVENTERINGSANMODAN UTSKRIVEN           
002600     03 MID-FLINVSKR-UT      PIC X.                                       
002700*                                 INVENTERINGSANMODAN UTSKRIVEN           
002800     03 MID-KVINVSKR-5309    PIC 9(2).                                    
002900*                                 BEGÄRDA INVENTERINGSUNDERLAG            
003000     03 MID-INV-ART-GRP      OCCURS 10 TIMES.                             
003100*                                 INVENTERINGSARTIKELGRUPP                
003200        05 MID-IDARTNR-UTSKR PIC 9(9).                                    
003300*                                 ARTIKELNUMMER                           
003400        05 MID-KDINVPRIO-UTSKR                                            
003500                             PIC X.                                       
003600*                                 INVENTERING PRIORITET                   
003700        05 MID-KDINVKAT-UTSKR                                             
003800                             PIC 9(2).                                    
003900*                                 INVENTERINGSKATEGORI                    
004000*** END OF VILMAII-COPY LENGTH= 148 BYTES                                 
