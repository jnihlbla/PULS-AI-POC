000100 01  MOD-W6O18501.                                                        
000200*                                 MOD COPYTEXT FÖR W60185                 
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDLEVNR-IN       PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER                        
000900     03 MOD-IDLEVNR-UT       PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100     03 MOD-IDARTNR-IN       PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 MOD-IDARTNR-UT       PIC X(9).                                    
001400*                                 ARTIKELNUMMER                           
001500     03 MOD-TYP-IN           PIC X.                                       
001600*                                 ALLMÄN SVARSFLAGGA                      
001700     03 MOD-TYP-UT           PIC X.                                       
001800*                                 ALLMÄN SVARSFLAGGA                      
001900     03 MOD-SUM-IN           PIC X.                                       
002000*                                 ALLMÄN SVARSFLAGGA                      
002100     03 MOD-SUM-UT           PIC X.                                       
002200*                                 ALLMÄN SVARSFLAGGA                      
002300     03 MOD-KVANTAL-ART      PIC Z(5)9.                                   
002400*                                 ANTAL                                   
002500     03 MOD-KVANTAL-FPINST   PIC Z(5)9.                                   
002600*                                 ANTAL                                   
002700     03 MOD-IDLTERM-ATTR     PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-IDLTERM          PIC X(8).                                    
003000*                                 LOGISKT TERMINALNAMN                    
003100     03 MOD-RAD              OCCURS 13 TIMES.                             
003200        05 MOD-CMD-ATTR      PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400        05 MOD-CMD           PIC X.                                       
003500        05 MOD-IDLEVNR       PIC X(5).                                    
003600*                                 LEVERANTÖRNUMMER                        
003700        05 MOD-IDARTNR       PIC Z(7)9.                                   
003800*                                 ARTIKELNUMMER                           
003900        05 MOD-BEFT          PIC Z9.                                      
004000*                                 FÖRPACKNINGSTYP                         
004100        05 MOD-REGTYP        PIC X(4).                                    
004200        05 MOD-IDFPINST      PIC Z(7).                                    
004300*                                 FÖRPACKNINGSINSTRUKTION NR              
004400        05 MOD-TIUPPDAT      PIC 9(6).                                    
004500*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
004600        05 MOD-IDPERSON      PIC Z(2)9.                                   
004700*                                 PERSONKOD                               
004800        05 MOD-BEINIT        PIC X(5).                                    
004900*                                 INITIALER FÖR EN PERSON                 
005000     03 MOD-TEMFSINF         PIC X(55).                                   
005100*                                 INFORMATIONSMEDDELANDE                  
005200*** END OF VILMAII-COPY LENGTH= 712 BYTES                                 
