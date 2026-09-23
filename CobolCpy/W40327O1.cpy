000100 01  RESP-W40327O1.                                                       
000200*                                 RESPONS FROM PGM W40327                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDPRC-KEY.                                                   
000600*                                 PRODUKTIONSKANAL                        
000700        05 RESP-IDPRCBAS     PIC X(3).                                    
000800*                                 PRC-BAS                                 
000900        05 RESP-IDPRCVAR     PIC X.                                       
001000*                                 PRC-VARIANT                             
001100     03 RESP-IDLOTNR-KEY     PIC Z(2)9.                                   
001200*                                 VAGN-NUMMER                             
001300     03 RESP-KVRADER-MAX1    PIC 9(5).                                    
001400*                                 MAX INDEX KOPPLAT TILL OCCURS N         
001500*                                 EDAN.                                   
001600     03 RESP-FLSKRIV-CLABEL  PIC X.                                       
001700*                                 J/Y = SKRIV BEGÄRD LISTA                
001800     03 RESP-FLSKRIV-DELNOTE PIC X.                                       
001900*                                 J/Y = SKRIV BEGÄRD LISTA                
002000     03 RESP-PRTVAL-ADRESSFL PIC X(2).                                    
002100*                                 PRINTER-VAL KOD ADRESS FLAGGA           
002200     03 RESP-PRTVAL-FOLJEFL  PIC X(2).                                    
002300*                                 PRINTER-VAL KOD FÖLJESEDEL              
002400     03 RESP-RAD             OCCURS 1 TO 500 TIMES                        
002500                             DEPENDING ON RESP-KVRADER-MAX1.              
002600*                                 COPYTEXT FÖR MOD WL0197O1               
002700        05 RESP-FLSKRIV      PIC X.                                       
002800*                                 ALLMÄN FLAGGA                           
002900        05 RESP-IDPRODNR     PIC Z(7).                                    
003000*                                 PRODUKTIONSNUMMER                       
003100        05 RESP-IDPLKLST     PIC Z(3).                                    
003200*                                 PLOCKLISTNUMMER                         
003300        05 RESP-IDKOLLI      PIC Z(5).                                    
003400*                                 KOLLINUMMER                             
003500        05 RESP-KDKOLLI      PIC X(8).                                    
003600*                                 KOLLIKOD                                
003700        05 RESP-IDDISTR      PIC Z(3)9.                                   
003800*                                 DISTRIKTNUMMER                          
003900        05 RESP-IDKUNDNR     PIC Z(5)9.                                   
004000*                                 KUNDNUMMER                              
004100        05 RESP-IDORDNR      PIC Z(4)9.                                   
004200*                                 ORDERNUMMER UTGÅR PD90                  
004300        05 RESP-KDKOLSTA     PIC 9.                                       
004400*                                 KOLLISTATUS                             
004500        05 RESP-KVORDRAD     PIC Z(4)9.                                   
004600*                                 ANTAL ORDERRADER                        
004700        05 RESP-KDFARLIG     PIC 9.                                       
004800*                                 KOD FÖR FARLIGT GODS                    
004900        05 RESP-VKORDBTO     PIC Z(5)9.9.                                 
005000*                                 ORDERVIKT BRUTTO (KG)                   
005100        05 RESP-VLORDBTO     PIC Z(3)9.9(3).                              
005200*                                 ORDERVOLYM BRUTTO (M3)                  
005300        05 RESP-DIKOLLIL     PIC Z(3)9.                                   
005400*                                 KOLLI-LÄNGD                             
005500        05 RESP-DIKOLLIH     PIC Z(2)9.                                   
005600*                                 KOLLI-HÖJD                              
005700        05 RESP-DIKOLLIB     PIC Z(2)9.                                   
005800*                                 KOLLI-BREDD                             
005900        05 RESP-IDTRPTNR     PIC Z(2)9.                                   
006000*                                 TRANSPORTIDENTITET                      
006100        05 RESP-IDMSG-ERROR-RAD                                           
006200                             PIC X(3).                                    
006300*                                 FELMEDDELANDE ID                        
006400*** END OF VILMAII-COPY LENGTH= 39020 BYTES                               
