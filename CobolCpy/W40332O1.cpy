000100 01  RESP-W40332O1.                                                       
000200*                                 RESP-COPYTEXT FÖR W40332                
000300*                                                                         
000400     03 RESP-IDDISTR         PIC X(4).                                    
000500*                                 DISTRIKTNUMMER                          
000600     03 RESP-IDKUNDNR        PIC X(6).                                    
000700*                                 KUNDNUMMER                              
000800     03 RESP-IDORDNR         PIC X(5).                                    
000900*                                 ORDERNUMMER UTGÅR PD90                  
001000     03 RESP-IDKOLLI         PIC X(5).                                    
001100*                                 KOLLINUMMER                             
001200     03 RESP-IDDC            PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 RESP-KDPRTVAL        PIC X(2).                                    
001500*                                 PRINTER-VAL KOD                         
001600     03 RESP-KDKOLLI         PIC X(8).                                    
001700*                                 KOLLIKOD                                
001800     03 RESP-KDEMBTYP        PIC Z(3).                                    
001900*                                 EMBALLAGETYP       KDEMBTYP-002         
002000     03 RESP-DIKOLLIL        PIC Z(5).                                    
002100*                                 KOLLI-LÄNGD                             
002200     03 RESP-DIKOLLIB        PIC Z(3).                                    
002300*                                 KOLLI-BREDD                             
002400     03 RESP-DIKOLLIH        PIC Z(3).                                    
002500*                                 KOLLI-HÖJD                              
002600     03 RESP-VKORDBTO        PIC Z(6).Z.                                  
002700*                                 ORDERVIKT BRUTTO (KG)                   
002800     03 RESP-ADRESS-TEXT     PIC X(11).                                   
002900     03 RESP-ADFLGEO         PIC X(3).                                    
003000*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
003100     03 RESP-ADFLOMR         PIC Z(2)9.                                   
003200*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
003300     03 RESP-ADRUTNIV        PIC Z(2)9.                                   
003400*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
003500     03 RESP-ADVMODUL        PIC Z(3).                                    
003600*                                 VÄNSTER-MODUL                           
003700     03 RESP-IDMFSINF        PIC X(3).                                    
003800*                                 MFS INFO. MEDDELANDE NUMMER             
003900     03 RESP-IDMSG-ERROR-LINE                                             
004000                             PIC X(55).                                   
004100*                                 INFORMATIONSMEDDELANDE                  
004200*** END OF VILMAII-COPY LENGTH= 135 BYTES                                 
