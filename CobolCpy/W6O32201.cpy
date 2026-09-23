000100 01  MOD-W6O32201.                                                        
000200*                                                                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-KDARBTYP-IN      PIC X(8).                                    
000800*                                 TYP AV ARBETE                           
000900     03 MOD-KDARBTYP-UT      PIC X(8).                                    
001000*                                 TYP AV ARBETE                           
001100     03 MOD-IDDC-IN          PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 MOD-IDDC-UT          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-IDPERSON-FOM-IN  PIC X(3).                                    
001600*                                 PERSONKOD                               
001700     03 MOD-IDPERSON-FOM-UT  PIC X(3).                                    
001800*                                 PERSONKOD                               
001900     03 MOD-IDPERSON-TOM-IN  PIC X(3).                                    
002000*                                 PERSONKOD                               
002100     03 MOD-IDPERSON-TOM-UT  PIC X(3).                                    
002200*                                 PERSONKOD                               
002300     03 MOD-IDARTNR-IN       PIC X(9).                                    
002400*                                 ARTIKELNUMMER                           
002500     03 MOD-IDARTNR-UT       PIC X(9).                                    
002600*                                 ARTIKELNUMMER                           
002700     03 MOD-TIDATUM-IN       PIC X(6).                                    
002800*                                 DATUM ENLIGT KDDATFORM                  
002900     03 MOD-TIDATUM-UT       PIC X(6).                                    
003000*                                 DATUM ENLIGT KDDATFORM                  
003100     03 MOD-FLKLAR-ATTR      PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300     03 MOD-FLKLAR           PIC X.                                       
003400*                                 AVSLUTNINGSMARKERING                    
003500     03 MOD-TABELLRAD        OCCURS 12 TIMES.                             
003600*                                 GRUPP MED TABELLRADER                   
003700        05 MOD-CMD-ATTR      PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900        05 MOD-CMD-IN        PIC X.                                       
004000        05 MOD-IDARTNR       PIC Z(8)9.                                   
004100*                                 ARTIKELNUMMER                           
004200        05 MOD-KVANTAL       PIC Z(7).                                    
004300*                                 ANTAL                                   
004400        05 MOD-SUARTSTD      PIC Z(6)9.9(2).                              
004500*                                 SUMMA STANDARDPRIS RADVÄRDE             
004600        05 MOD-TIDATUM       PIC 9(6).                                    
004700*                                 DATUM ENLIGT KDDATFORM                  
004800        05 MOD-IDDC          PIC X(2).                                    
004900*                                 IDENTIFIERARE LAGER                     
005000        05 MOD-IDPERSON      PIC Z(2)9.                                   
005100*                                 PERSONKOD                               
005200        05 MOD-REM           PIC X.                                       
005300        05 MOD-IDUSER        PIC X(8).                                    
005400*                                 ANVÄNDARENS SÄKERHETS ID                
005500        05 MOD-ACC           PIC X.                                       
005600        05 MOD-FLCLASS       PIC X.                                       
005700*                                 ALLMÄN FLAGGA                           
005800        05 MOD-FLTEXT        PIC X.                                       
005900*                                 ALLMÄN FLAGGA                           
006000     03 MOD-TEMFSINF         PIC X(55).                                   
006100*                                 INFORMATIONSMEDDELANDE                  
006200*** END OF VILMAII-COPY LENGTH= 788 BYTES                                 
