000100 01  MID-W6I10601.                                                        
000200*                                 COPYTEXT FÖR MID                        
000300*                                 W6I10601                                
000400     03 MID-ADINLOMR-IN      PIC X(4).                                    
000500*                                 INLEVERANSOMRÅDE                        
000600     03 MID-ADINLOMR-UT      PIC X(4).                                    
000700*                                 INLEVERANSOMRÅDE                        
000800     03 MID-KDINLOMR-IN      PIC X(3).                                    
000900*                                 TYP AV INLEVERANSOMRÅDE                 
001000     03 MID-KDINLOMR-UT      PIC X(3).                                    
001100*                                 TYP AV INLEVERANSOMRÅDE                 
001200     03 MID-IDDC-IN          PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 MID-IDDC-UT          PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 MID-INPUT.                                                        
001700*                                 INDATA FÖR UPPDATERING                  
001800        05 MID-ADINLOMR-PAR  PIC X(4).                                    
001900*                                 INLEVERANSOMRÅDE ÖVERORDNAT             
002000        05 MID-KDINLUPF      PIC X(4).                                    
002100*                                 UPPFÖLJNINGSSTATUS INLEVERANS           
002200        05 MID-ADINLOMR-BO   PIC X(4).                                    
002300*                                 BUFFERTOMRÅDE                           
002400        05 MID-ADINLOMR-LPL  PIC X(4).                                    
002500*                                 LOSSNINGSPLATS                          
002600        05 MID-KDLORAPP      PIC X.                                       
002700*                                 KOD FÖR R32-RAPPORTERING                
002800        05 MID-TEXT1         PIC X(17).                                   
002900        05 MID-ADPLATS-FOM   PIC X(5).                                    
003000*                                 LAGERPLATSNUMMER                        
003100        05 MID-ADPLATS-TOM   PIC X(5).                                    
003200*                                 LAGERPLATSNUMMER                        
003300        05 MID-ADGANG-FOM    PIC X(2).                                    
003400*                                 GÅNG                                    
003500        05 MID-ADGANG-TOM    PIC X(2).                                    
003600*                                 GÅNG                                    
003700        05 MID-FLLOLL        PIC X.                                       
003800*                                 SKAPA LOSSNINGSLISTEFLAGGA              
003900        05 MID-FLCDOMR       PIC X.                                       
004000*                                 FLAGGA CROSS DOCKING OMRÅDE             
004100        05 MID-IDPERSON-ANSV PIC 9(3).                                    
004200*                                 PERSONKOD                               
004300        05 MID-IDLEVNR       PIC X(5).                                    
004400*                                 LEVERANTÖRNUMMER                        
004500        05 MID-IDPERSON-FORP PIC 9(3).                                    
004600*                                 PERSONKOD                               
004700        05 MID-ADINLOMR-PRT  PIC X(4).                                    
004800*                                 PRINTERPLACERING                        
004900        05 MID-IDPERSON-KVAL PIC 9(3).                                    
005000*                                 PERSONKOD                               
005100        05 MID-IDAVD-DAG     PIC X(5).                                    
005200*                                 DEN ANSTÄLLDES AVDELNING/DAG            
005300        05 MID-IDGRUPP-DAG   PIC X(2).                                    
005400*                                 DEN ANSTÄLLDES GRUPPID/DAG              
005500        05 MID-IDAVD-NATT    PIC X(5).                                    
005600*                                 DEN ANSTÄLLDES AVDELNING/NATT           
005700        05 MID-IDGRUPP-NATT  PIC X(2).                                    
005800*                                 DEN ANSTÄLLDES GRUPPID/NATT             
005900        05 MID-FLKVARED      PIC X.                                       
006000*                                 REDUCERAD KONTROLL FLAGGA               
006100        05 MID-FLKNTRGK      PIC X.                                       
006200*                                 OMPLACERING GODK. KONTROLL J/N          
006300        05 MID-FLEXCP        PIC X.                                       
006400*                                 ALLMÄN FLAGGA FÖR UNDANTAG              
006500        05 MID-KVTID-NORM    PIC 9(4).                                    
006600*                                 NORMAL MÅLTID FÖR GODSPLACERING         
006700        05 MID-KVTID-NORMTOT PIC 9(4).                                    
006800*                                 NORMTOTAL MÅLTID GODSPLACERING          
006900        05 MID-KVTID-PRIO    PIC 9(4).                                    
007000*                                 PRIO MÅLTID FÖR GODSPLACERING           
007100        05 MID-KVTID-PRIOTOT PIC 9(4).                                    
007200*                                 PRIOTOTAL MÅLTID GODSPLACERING          
007300        05 MID-KVTID-NTCDC   PIC 9(4).                                    
007400*                                 NORMTOTAL MÅLTID GODSPLAC.(CDC)         
007500        05 MID-KVTID-PTCDC   PIC 9(4).                                    
007600*                                 PRIOTOTAL MÅLTID GODSPLAC.(CDC)         
007700        05 MID-KVTID-NTSVS   PIC 9(4).                                    
007800*                                 NORMTOTAL MÅLTID GODSPLAC.(SVS)         
007900        05 MID-KVTID-PTSVS   PIC 9(4).                                    
008000*                                 PRIOTOTAL MÅLTID GODSPLAC.(SVS)         
008100     03 MID-FLSVAR           PIC X.                                       
008200*                                 ALLMÄN SVARSFLAGGA                      
008300*** END OF VILMAII-COPY LENGTH= 136 BYTES                                 
