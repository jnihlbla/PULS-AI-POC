000100 01  MOD-W4O22601.                                                        
000200*                                                                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MOD-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001200*                                 KUNDNUMMER                              
001300     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MOD-IDARTNR-IN       PIC X(9).                                    
001600*                                 ARTIKELNUMMER                           
001700     03 MOD-IDARTNR-UT       PIC X(9).                                    
001800*                                 ARTIKELNUMMER                           
001900     03 MOD-RAD              OCCURS 14 TIMES.                             
002000*                                 GRUPP MED RADER                         
002100        05 MOD-KDCMDVAL-ATTR PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300        05 MOD-KDCMDVAL      PIC X(3).                                    
002400*                                 GENERELL KOMMANDOKOD                    
002500        05 MOD-TIREGDAT-URSP PIC 9(6).                                    
002600*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002700        05 MOD-TEVORMRK      PIC X(2).                                    
002800*                                 MÄRKNINGSTEXT FÖR                       
002900*                                 VOR-KÖN                                 
003000        05 MOD-TEVORMRK-SC   PIC X(2).                                    
003100*                                 MÄRKNINGSTEXT FÖR                       
003200*                                 VOR-KÖN                                 
003300        05 MOD-IDDISTR       PIC Z(3)9.                                   
003400*                                 DISTRIKTNUMMER                          
003500        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
003600*                                 KUNDNUMMER                              
003700        05 MOD-IDORDNR7      PIC Z(6)9.                                   
003800*                                 ORDERNUMMER                             
003900        05 MOD-IDARTNR       PIC Z(8)9.                                   
004000*                                 ARTIKELNUMMER                           
004100        05 MOD-KVBEART-URSP  PIC Z(6)9.                                   
004200*                                 BESTÄLLT ANTAL STYCKEN                  
004300        05 MOD-KVBEART       PIC Z(6)9.                                   
004400*                                 BESTÄLLT ANTAL STYCKEN                  
004500        05 MOD-KDORDBEK      PIC 9(2).                                    
004600*                                 ORDERBEKRÄFTELSEKOD                     
004700        05 MOD-IDDC          PIC X(2).                                    
004800*                                 IDENTIFIERARE LAGER                     
004900        05 MOD-FLAGGA-VOR-ATTR                                            
005000                             PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200        05 MOD-FLAGGA-VOR    PIC X.                                       
005300*                                 ALLMÄN FLAGGA                           
005400        05 MOD-FLAGGA-SC     PIC X.                                       
005500*                                 ALLMÄN FLAGGA                           
005600        05 MOD-FLAGGA-DEAL   PIC X.                                       
005700*                                 ALLMÄN FLAGGA                           
005800     03 MOD-TEMFSINF         PIC X(55).                                   
005900*                                 INFORMATIONSMEDDELANDE                  
006000*** END OF VILMAII-COPY LENGTH= 1033 BYTES                                
