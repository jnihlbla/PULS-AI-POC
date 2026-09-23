000100 01  W412RX5-CTX.                                                         
000200*                                 TYP = RX5, LÄNGD = 80                   
000300*                                                                         
000400     03 IDTYP                PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDDISTR              PIC 9(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800     03 IDKUNDNR             PIC 9(6).                                    
000900*                                 KUNDNUMMER                              
001000     03 IDORDNR7             PIC 9(7).                                    
001100*                                 ORDERNUMMER                             
001200     03 KDORDKL              PIC 9.                                       
001300*                                 ORDERKLASS                              
001400     03 KDVRINFO             PIC 9.                                       
001500*                                 PÅVERKAN I VR/DSP SYSTEM                
001600     03 KDTPOTYP             PIC 9.                                       
001700*                                 TYP AV TIDPLANERAD ORDER                
001800     03 IDARTNR              PIC X(9).                                    
001900*                                 ARTIKELNUMMER                           
002000     03 REKSIFFR             PIC 9.                                       
002100*                                 KONTROLLSIFFRA                          
002200     03 KVBEART              PIC 9(6).                                    
002300*                                 BESTÄLLT ANTAL STYCKEN                  
002400     03 KDKVBRYT             PIC 9.                                       
002500*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
002600     03 TITPO                PIC 9(6).                                    
002700*                                 PLANERAD ORDERDATUM                     
002800     03 BERADREF             PIC X(10).                                   
002900*                                 KUNDENS RADREFERENS                     
003000     03 FLSLATT              PIC X.                                       
003100*                                 FLAGGA SOM ANGER OM KVSLATT SKA         
003200*                                 LL BERÄKNAS ELLER EJ                    
003300*                                 OM FLRESTN = J OCH FLSLATT = J,         
003400*                                  DÅ BERÄKNAS KVSLATT                    
003500     03 FILLERX26            PIC X(26).                                   
003600     03 FTGKOD               PIC X(2).                                    
003700     03 KDMASK               PIC X.                                       
003800*                                 KOD FÖR MASKINELL ORDERRAD              
003900*                                 H = SATS            W221                
004000*                                 R = RO/DO           W441                
004100*                                 S = SATS            R241                
004200*                                 D = ORDERRAD        D886                
004300*                                 B = BIPACKAD RO/DO  W411                
004400*** END OF VILMAII-COPY LENGTH= 86 BYTES                                  
