000100 01  MOD-W4O30401-CTX.                                                    
000200*                                 MODCOPYTEXT TILL W40304.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDPRODNR-IN      PIC Z(7).                                    
000800*                                 PRODUKTIONSNUMMER                       
000900     03 MOD-IDPRODNR-UT      PIC Z(7).                                    
001000*                                 PRODUKTIONSNUMMER                       
001100     03 MOD-TEDDI            PIC X(11).                                   
001200*                                 TEXTFÄLT DDI                            
001300     03 MOD-IDARTNR          PIC Z(8)9.                                   
001400*                                 ARTIKELNUMMER                           
001500     03 MOD-BEART            PIC X(25).                                   
001600*                                 ARTIKELBENÄMNING                        
001700     03 MOD-KVBEART          PIC Z(7).                                    
001800*                                 BESTÄLLT ANTAL STYCKEN                  
001900     03 MOD-W4O30401-001-GRP.                                             
002000*                                 UPDATE                                  
002100        05 MOD-W4O30401-002-GRP                                           
002200                             OCCURS 11 TIMES.                             
002300*                                 UPDATE                                  
002400           07 MOD-ATTR-IDPURAD1                                           
002500                             PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700           07 MOD-IDPURAD1   PIC X(4).                                    
002800*                                 RADNUMMER PÅ PACKUNDERLAG               
002900        05 MOD-W4O30401-003-GRP                                           
003000                             OCCURS 11 TIMES.                             
003100*                                 UPDATE                                  
003200           07 MOD-ATTR-REBEART1                                           
003300                             PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500           07 MOD-REBEART1   PIC X(7).                                    
003600*                                 ANTAL PER ORDERRAD SATS                 
003700        05 MOD-W4O30401-004-GRP                                           
003800                             OCCURS 11 TIMES.                             
003900*                                 UPDATE                                  
004000           07 MOD-ATTR-IDPURAD2                                           
004100                             PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300           07 MOD-IDPURAD2   PIC X(4).                                    
004400*                                 RADNUMMER PÅ PACKUNDERLAG               
004500        05 MOD-W4O30401-005-GRP                                           
004600                             OCCURS 11 TIMES.                             
004700*                                 UPDATE                                  
004800           07 MOD-ATTR-REBEART2                                           
004900                             PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100           07 MOD-REBEART2   PIC X(7).                                    
005200*                                 ANTAL PER ORDERRAD SATS                 
005300        05 MOD-W4O30401-006-GRP                                           
005400                             OCCURS 11 TIMES.                             
005500*                                 UPDATE                                  
005600           07 MOD-ATTR-IDPURAD3                                           
005700                             PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900           07 MOD-IDPURAD3   PIC X(4).                                    
006000*                                 RADNUMMER PÅ PACKUNDERLAG               
006100        05 MOD-W4O30401-007-GRP                                           
006200                             OCCURS 11 TIMES.                             
006300*                                 UPDATE                                  
006400           07 MOD-ATTR-REBEART3                                           
006500                             PIC X(2).                                    
006600*                                 MFS ATTRIBUTFÄLT                        
006700           07 MOD-REBEART3   PIC X(7).                                    
006800*                                 ANTAL PER ORDERRAD SATS                 
006900     03 MOD-TEMFSINF         PIC X(55).                                   
007000*                                 INFORMATIONSMEDDELANDE                  
007100*** END OF VILMAII-COPY LENGTH= 660 BYTES                                 
