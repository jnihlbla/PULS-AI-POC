000100 01  MID-W6I21701.                                                        
000200*                                 MID TILL W60217                         
000300     03 MID-IDPERSON-QUAL-IN PIC X(3).                                    
000400*                                 PERSONKOD QUALITY                       
000500     03 MID-IDPERSON-QUAL-UT PIC X(3).                                    
000600*                                 PERSONKOD QUALITY                       
000700     03 MID-IDPERSON-PACK-IN PIC X(3).                                    
000800*                                 PERSONKOD CDC                           
000900     03 MID-IDPERSON-PACK-UT PIC X(3).                                    
001000*                                 PERSONKOD CDC                           
001100     03 MID-KDARBVAL-IN      PIC X.                                       
001200*                                 ARBETSTYPSVAL KOD                       
001300     03 MID-KDARBVAL-UT      PIC X.                                       
001400*                                 ARBETSTYPSVAL KOD                       
001500     03 MID-IDLANDX2-IN      PIC X(2).                                    
001600*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001700     03 MID-IDLANDX2-UT      PIC X(2).                                    
001800*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001900     03 MID-RAD              OCCURS 12 TIMES.                             
002000*                                 MID TILL W60217                         
002100        05 MID-KDCMDVAL      PIC X(3).                                    
002200*                                 GENERELL KOMMANDOKOD                    
002300        05 MID-IDPERSON-QUAL PIC 9(3).                                    
002400*                                 PERSONKOD QUALITY                       
002500        05 MID-IDPERSON-PACK PIC 9(3).                                    
002600*                                 PERSONKOD CDC                           
002700        05 MID-IDARTNR-FOM   PIC 9(9).                                    
002800*                                 ARTIKELNUMMER                           
002900        05 MID-IDARTNR-TOM   PIC 9(9).                                    
003000*                                 ARTIKELNUMMER                           
003100        05 MID-IDLEVNR       PIC X(5).                                    
003200*                                 LEVERANTÖRNUMMER                        
003300        05 MID-IDFKNGRP-FOM  PIC 9(4).                                    
003400*                                 FUNKTIONSGRUPP                          
003500        05 MID-IDFKNGRP-TOM  PIC 9(4).                                    
003600*                                 FUNKTIONSGRUPP                          
003700     03 MID-IDPERSON-QUAL-NY PIC X(3).                                    
003800*                                 PERSONKOD QUALITY                       
003900     03 MID-IDPERSON-PACK-NY PIC X(3).                                    
004000*                                 PERSONKOD CDC                           
004100     03 MID-IDARTNR-FOM-NY   PIC X(9).                                    
004200*                                 ARTIKELNUMMER                           
004300     03 MID-IDARTNR-TOM-NY   PIC X(9).                                    
004400*                                 ARTIKELNUMMER                           
004500     03 MID-IDLEVNR-NY       PIC X(5).                                    
004600*                                 LEVERANTÖRNUMMER                        
004700     03 MID-IDFKNGRP-FOM-NY  PIC X(4).                                    
004800*                                 FUNKTIONSGRUPP                          
004900     03 MID-IDFKNGRP-TOM-NY  PIC X(4).                                    
005000*                                 FUNKTIONSGRUPP                          
005100     03 MID-IDLANDX2-NY      PIC X(2).                                    
005200*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
005300*** END OF VILMAII-COPY LENGTH= 537 BYTES                                 
