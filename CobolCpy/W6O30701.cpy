000100 01  MOD-W6O30701.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD                   
000300*                                 GODSMOTTAGNINGSHISTORIK                 
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDARTNR-IN       PIC X(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 MOD-IDARTNR-UT       PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200     03 MOD-IDDC-IN          PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 MOD-IDDC-UT          PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 MOD-BEART            PIC X(25).                                   
001700*                                 ARTIKELBENÄMNING                        
001800     03 MOD-FLMORE-ATTR      PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000     03 MOD-FLMORE           PIC X.                                       
002100     03 MOD-INFO-RAD         OCCURS 13 TIMES.                             
002200*                                 RADINFORMATION                          
002300        05 MOD-IDPTYP        PIC X(3).                                    
002400*                                 POSTTYP                                 
002500        05 MOD-IDDC          PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700        05 MOD-IDLEVNR       PIC X(5).                                    
002800*                                 LEVERANTÖRNUMMER                        
002900        05 MOD-KDRT          PIC Z9.                                      
003000*                                 REDOVISNINGSTYP                         
003100        05 MOD-IDLOPNRM      PIC Z(7)9.                                   
003200*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
003300*                                 (0VVDLLLLK)                             
003400        05 MOD-IDKUNDRF      PIC X(10).                                   
003500*                                 KUNDENS REFERENS (ORDERID)              
003600        05 MOD-TIREGDAT      PIC 9(6).                                    
003700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003800        05 MOD-TIINLINL      PIC 9(6).                                    
003900*                                 RAPPORTERINGSDATUM INLAGD (R32)         
004000        05 MOD-KVAVIS        PIC -(6)9.                                   
004100*                                 AVISERAT ANTAL                          
004200        05 MOD-KVANTMOT      PIC -(6)9.                                   
004300*                                 ANTAL MOTTAGET                          
004400        05 MOD-KVART-SKROT   PIC Z(6)9.                                   
004500*                                 ANTAL SKROTADE ARTIKLAR                 
004600        05 MOD-FLMAKUL       PIC X.                                       
004700*                                 FLAGGA MAKULERAT KOLLI                  
004800     03 MOD-TEMFSINF         PIC X(55).                                   
004900*                                 INFORMATIONSMEDDELANDE                  
005000*** END OF VILMAII-COPY LENGTH= 981 BYTES                                 
