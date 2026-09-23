000100 01  W412RX7-CTX.                                                         
000200*                                 TYP = RX7, LÄNGD = 136                  
000300*                                                                         
000400*                                                                         
000500     03 IDTYP                PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 IDDISTR              PIC 9(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 IDKUNDNR             PIC 9(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 IDORDNR7             PIC 9(7).                                    
001200*                                 ORDERNUMMER                             
001300     03 KDORDKL              PIC 9.                                       
001400*                                 ORDERKLASS                              
001500     03 KDVRINFO             PIC 9.                                       
001600*                                 PÅVERKAN I VR/DSP SYSTEM                
001700     03 KDTPOTYP             PIC 9.                                       
001800*                                 TYP AV TIDPLANERAD ORDER                
001900     03 IDARTNR              PIC X(9).                                    
002000*                                 ARTIKELNUMMER                           
002100     03 REKSIFFR             PIC 9.                                       
002200*                                 KONTROLLSIFFRA                          
002300     03 KVBEART              PIC 9(6).                                    
002400*                                 BESTÄLLT ANTAL STYCKEN                  
002500     03 KDKVBRYT             PIC 9.                                       
002600*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
002700     03 TITPO                PIC 9(6).                                    
002800*                                 PLANERAD ORDERDATUM                     
002900     03 BERADREF             PIC X(10).                                   
003000*                                 KUNDENS RADREFERENS                     
003100     03 FLSLATT              PIC X.                                       
003200*                                 FLAGGA SOM ANGER OM KVSLATT SKA         
003300*                                 LL BERÄKNAS ELLER EJ                    
003400*                                 OM FLRESTN = J OCH FLSLATT = J,         
003500*                                  DÅ BERÄKNAS KVSLATT                    
003600     03 FILLERX20            PIC X(20).                                   
003700     03 FTGKOD               PIC X(2).                                    
003800     03 KDMASK               PIC X.                                       
003900*                                 KOD FÖR MASKINELL ORDERRAD              
004000*                                 H = SATS            W221                
004100*                                 R = RO/DO           W441                
004200*                                 S = SATS            R241                
004300*                                 D = ORDERRAD        D886                
004400*                                 B = BIPACKAD RO/DO  W411                
004500     03 PRARTBTO-LOC         PIC 9(7)V9(2).                               
004600*                                 PRIS I LOKAL VALUTA                     
004700     03 PRARTNTO-LOC         PIC 9(7)V9(2).                               
004800*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
004900     03 KDVALISO             PIC X(3).                                    
005000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005100     03 KDVAT                PIC X(2).                                    
005200*                                 MOMSKOD                                 
005300     03 RERAB                PIC 9(2)V9(1).                               
005400*                                 RABATTSATS (PROCENT)                    
005500     03 KDRAB                PIC X(5).                                    
005600*                                 RABATTKOD                               
005700     03 BEART-VIPS           PIC X(25).                                   
005800*                                 VIPS ARTIKELBENÄMNING                   
005900*                                 PÅ DEALERNS SPRÅK                       
006000*** END OF VILMAII-COPY LENGTH= 136 BYTES                                 
