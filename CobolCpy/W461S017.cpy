000100 01  OBERS-W461S017.                                                      
000200*                                 SAMMANSLAGNING AV SORTDEL OCH           
000300*                                 ORDERBEKRÄFTELSE ERSÄTTNING             
000400*                                 TILL NOAC                               
000500     03 OBERS-SOR0-IDDISTR   PIC S9(5)           COMP-3.                  
000600*                                 DISTRIKTNUMMER                          
000700     03 OBERS-SOR0-IDKUNDNR  PIC S9(7)           COMP-3.                  
000800*                                 KUNDNUMMER                              
000900     03 OBERS-SOR0-IDRONR    PIC S9(7)           COMP-3.                  
001000*                                 RESTORDERNUMMER      IDRONR-002         
001100     03 OBERS-SOR0-TIRODAT   PIC S9(7)           COMP-3.                  
001200*                                 RESTORDERDATUM         (ÅÅMMDD)         
001300     03 OBERS-SOR0-IDPTYP    PIC X(3).                                    
001400*                                 POSTTYP                                 
001500     03 OBERS-SOR0-IDLOPNR   PIC S9(5)           COMP-3.                  
001600*                                 LÖPNUMMER          IDLOPNR-002          
001700     03 OBERS-W461017.                                                    
001800*                                 ORDERBEKRÄFTELSE ERS                    
001900*                                 TILL NOAC PT-017                        
002000        05 OBERS-IDPTYP      PIC X(3).                                    
002100*                                 POSTTYP                                 
002200        05 OBERS-IDDISTR     PIC S9(5)           COMP-3.                  
002300*                                 DISTRIKTNUMMER                          
002400        05 OBERS-IDKUNDNR    PIC S9(7)           COMP-3.                  
002500*                                 KUNDNUMMER                              
002600        05 OBERS-KDFRAKT     PIC S9(3)           COMP-3.                  
002700*                                 FRAKTSÄTT C1-C2 TILL KUND               
002800        05 OBERS-IDORDNR     PIC S9(7)           COMP-3.                  
002900*                                 ORDERNR             IDORDNR-002         
003000        05 OBERS-IDLOPNR-RAD PIC S9(3)           COMP-3.                  
003100*                                 LÖPNUMMER                               
003200        05 OBERS-KDTILLK     PIC S9              COMP-3.                  
003300*                                 TILLKOMMANDE-KOD                        
003400        05 OBERS-KDRADERS    PIC S9              COMP-3.                  
003500*                                 HUR RADEN ÄR ERSATT                     
003600        05 OBERS-KDCLAGER    PIC S9              COMP-3.                  
003700*                                 CENTRALLAGERKOD                         
003800        05 OBERS-IDARTNR     PIC S9(9)           COMP-3.                  
003900*                                 ARTIKELNUMMER                           
004000        05 OBERS-REKSIFFR    PIC S9              COMP-3.                  
004100*                                 KONTROLLSIFFRA                          
004200        05 OBERS-BEART       PIC X(25).                                   
004300*                                 ARTIKELBENÄMNING                        
004400        05 OBERS-BERADREF    PIC X(10).                                   
004500*                                 KUNDENS RADREFERENS                     
004600        05 OBERS-IDRONR      PIC S9(7)           COMP-3.                  
004700*                                 RESTORDERNUMMER      IDRONR-002         
004800        05 OBERS-TIRODAT     PIC S9(7)           COMP-3.                  
004900*                                 RESTORDERDATUM         (ÅÅMMDD)         
005000        05 OBERS-BEVOLREF    PIC X(10).                                   
005100*                                 VOLVO REFERENS                          
005200        05 OBERS-KDRESTR     PIC S9(3)           COMP-3.                  
005300*                                 RESTRIKTIONSKOD                         
005400        05 OBERS-KDERS       PIC S9(3)           COMP-3.                  
005500*                                 ERSÄTTNINGSKOD                          
005600        05 OBERS-KVBEART     PIC S9(7)           COMP-3.                  
005700*                                 BESTÄLLT ANTAL ARTIKLAR                 
005800        05 OBERS-KDERSUP     PIC S9              COMP-3.                  
005900*                                 UPPDATERING AV IMPORTÖRS ARTREG         
006000*                                 VID ERSÄTTNING                          
006100        05 OBERS-DIERS-KVOT  PIC S9(4)V9(3)      COMP-3.                  
006200*                                 KVOT MELLAN                             
006300*                                 DIERS-TILLK OCH DIERS-ERS               
006400        05 OBERS-KDKVBRYT    PIC S9              COMP-3.                  
006500*                                 KOD OM KVANFÖRP SKALL BRYTAS            
006600        05 OBERS-KDDSP       PIC S9              COMP-3.                  
006700*                                 PÅVERKAN PÅ DSP                         
006800        05 FILLER            PIC X(6).                                    
006900*** END COPY W461S017C0  LENGTH=122                                       
