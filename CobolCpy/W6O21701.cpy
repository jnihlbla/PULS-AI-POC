000100 01  MOD-W6O21701.                                                        
000200*                                 MOD TILL W60217                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDPERSON-QUAL-IN PIC X(3).                                    
000800*                                 PERSONKOD QUALITY                       
000900     03 MOD-IDPERSON-QUAL-UT PIC X(3).                                    
001000*                                 PERSONKOD QUALITY                       
001100     03 MOD-IDPERSON-PACK-IN PIC X(3).                                    
001200*                                 PERSONKOD CDC                           
001300     03 MOD-IDPERSON-PACK-UT PIC X(3).                                    
001400*                                 PERSONKOD CDC                           
001500     03 MOD-KDARBVAL-IN      PIC X.                                       
001600*                                 ARBETSTYPSVAL KOD                       
001700     03 MOD-KDARBVAL-UT      PIC X.                                       
001800*                                 ARBETSTYPSVAL KOD                       
001900     03 MOD-IDLANDX2-IN      PIC X(2).                                    
002000*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
002100     03 MOD-IDLANDX2-UT      PIC X(2).                                    
002200*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
002300     03 MOD-RAD              OCCURS 12 TIMES.                             
002400        05 MOD-KDCMDVAL-ATTR PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600        05 MOD-KDCMDVAL      PIC X(3).                                    
002700*                                 GENERELL KOMMANDOKOD                    
002800        05 MOD-IDPERSON-QUAL PIC Z(2)9.                                   
002900*                                 PERSONKOD                               
003000        05 MOD-IDPERSON-PACK PIC Z(2)9.                                   
003100*                                 PERSONKOD                               
003200        05 MOD-IDARTNR-FOM   PIC Z(9).                                    
003300*                                 ARTIKELNUMMER                           
003400        05 MOD-IDARTNR-TOM   PIC Z(9).                                    
003500*                                 ARTIKELNUMMER                           
003600        05 MOD-IDLEVNR       PIC X(5).                                    
003700*                                 LEVERANTÖRNUMMER                        
003800        05 MOD-IDFKNGRP-FOM  PIC Z(4).                                    
003900*                                 FUNKTIONSGRUPP                          
004000        05 MOD-IDFKNGRP-TOM  PIC Z(4).                                    
004100*                                 FUNKTIONSGRUPP                          
004200     03 MOD-IDPERSON-QUAL-ATTR                                            
004300                             PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500     03 MOD-IDPERSON-QUAL-NY PIC X(3).                                    
004600*                                 PERSONKOD                               
004700     03 MOD-IDPERSON-PACK-ATTR                                            
004800                             PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000     03 MOD-IDPERSON-PACK-NY PIC X(3).                                    
005100*                                 PERSONKOD                               
005200     03 MOD-IDARTNR-FOM-ATTR PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400     03 MOD-IDARTNR-FOM-NY   PIC Z(9).                                    
005500*                                 ARTIKELNUMMER                           
005600     03 MOD-IDARTNR-TOM-ATTR PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800     03 MOD-IDARTNR-TOM-NY   PIC Z(9).                                    
005900*                                 ARTIKELNUMMER                           
006000     03 MOD-IDLEVNR-ATTR     PIC X(2).                                    
006100*                                 MFS ATTRIBUTFÄLT                        
006200     03 MOD-IDLEVNR-NY       PIC X(5).                                    
006300*                                 LEVERANTÖRNUMMER                        
006400     03 MOD-IDFKNGRP-FOM-ATTR                                             
006500                             PIC X(2).                                    
006600*                                 MFS ATTRIBUTFÄLT                        
006700     03 MOD-IDFKNGRP-FOM-NY  PIC Z(4).                                    
006800*                                 FUNKTIONSGRUPP                          
006900     03 MOD-IDFKNGRP-TOM-ATTR                                             
007000                             PIC X(2).                                    
007100*                                 MFS ATTRIBUTFÄLT                        
007200     03 MOD-IDFKNGRP-TOM-NY  PIC Z(4).                                    
007300*                                 FUNKTIONSGRUPP                          
007400     03 MOD-IDLANDX2-ATTR    PIC X(2).                                    
007500*                                 MFS ATTRIBUTFÄLT                        
007600     03 MOD-IDLANDX2-NY      PIC X(2).                                    
007700*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
007800     03 MOD-TEMFSINF         PIC X(55).                                   
007900*                                 INFORMATIONSMEDDELANDE                  
008000*** END OF VILMAII-COPY LENGTH= 676 BYTES                                 
