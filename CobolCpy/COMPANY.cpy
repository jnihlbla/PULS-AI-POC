000100* GENERATION OF COBOL HOST STRUCTURE FROM COMPANY-TAB                     
000200  01 COMPANY.                                                             
000300*              ALL REPORTED COMPANYS SALES VALUES                         
000400   03 IDLANDX2                          PIC X(2).                         
000500*              2-STÄLLIG LANDSBETECKNINGSKOD                              
000600   03 IDMARKBO                          PIC X(2).                         
000700*              AMC MARKNADSBOLAGSKOD                                      
000800   03 KDSYINST                          PIC X(1).                         
000900*              SYSTEM INSTALLATION KOD                                    
001000   03 BELAND                            PIC X(12).                        
001100*              LANDSBETECKNING                                            
001200   03 BEIMP                             PIC X(30).                        
001300*              IMPORTÖRSNAMN                                              
001400   03 KDVALISO                          PIC X(3).                         
001500*              VALUTAKOD ENLIGT ISO-STANDARD.                             
001600   03 KDIMPTYP                          PIC X(2).                         
001700*              IMPORTÖRSTYP                                               
001800   03 KDIMPSTA                          PIC X(2).                         
001900*              STATISTIK KOD                                              
002000   03 KVROUND                           PIC S9(7) COMP-3.                 
002100*              RUNDNINGSFAKTOR ELLER RUNDNINGSANTAL                       
002200*              ANGER HUR VOLYMER EL PRISER SKA RUNDAS                     
002300*              TEX PER 100 ELLER PER 1000                                 
002400   03 FLCOSTAV                          PIC X(1).                         
002500*              FLAGGA COST TILLGÄNGLIG                                    
002600   03 KDCALCPR                          PIC X(1).                         
002700*              BERÄKNINGSKOD                                              
002800   03 FLTOTLIN-VCC                      PIC X(1).                         
002900*              INDIKERAR TOTALRAD VCC                                     
003000   03 FLTOTLIN-NEDCAR                   PIC X(1).                         
003100*              INDIKERAR TOTALRAD NEDC                                    
003200   03 FLTOTLIN-RENAULT                  PIC X(1).                         
003300*              INDIKERAR TOTALRAD RENÅ                                    
003400*                                                                         
003500*** END OF VILMAII-COPY LENGTH= 63 OLD LENGTH=                            
