000100*                                 VTAB - INTRASTAT                        
000200*                                 BESTÅR AV 4 POSTTYPER:                  
000300*                                  01 - FILAVSÄNDARINFORMATION            
000400*                                  02 - ARTIKELINFORMATION                
000500*                                  03 - FAKTURAINFORMATION                
000600*                                       (OM ARTIKELINFO SAKNAS)           
000700*                                  04 - VARUPOSTINFORMATION               
000800*                                 SAMTLIGA ALFA-FÄLT SKALL VARA           
000900*                                 VÄNSTERJUSTERADE.                       
001000 01  FORSEL-AREA.                                                         
001100     03 POSTTYP                   PIC X(2).                               
001200*                                                                         
001300     03 FIL-AREA.                                                         
001400*                                 FILUPPGIFTER POSTTYP 01                 
001500        05 FILAVSID               PIC X(8).                               
001600        05 FILAVSNAMN             PIC X(35).                              
001700        05 AVSORGNR               PIC X(17).                              
001800*          REDOVISNINGSSKYLDIGT BOLAG                                     
001900        05 FILDATUM               PIC X(8).                               
002000        05 FILTID                 PIC X(4).                               
002100        05 PERIOD                 PIC X(6).                               
002200*          ANGES SOM ÅÅÅÅMM (EX. 199501)                                  
002300        05 FORSELKOD              PIC X(1).                               
002400*          TILLÅTNA VÄRDEN: I, U                                          
002500        05 FILLER                 PIC X(18).                              
002600*                                                                         
002700     03 ARTIKEL-AREA              REDEFINES FIL-AREA.                     
002800*                                 ARTIKELUPPGIFTER POSTTYP 02             
002900        05 LANDKOD                PIC X(2).                               
003000        05 TRANSPORTSATT          PIC X(1).                               
003100*          ANGES ENDAST VID ÖVERENSKOMMELSE MED VTAB,                     
003200*          ANNARS BLANK                                                   
003300        05 ARTIKELNR              PIC X(14).                              
003400        05 ANTALSTYP              PIC X(4).                               
003500        05 ANTAL                  PIC S9(12).                             
003600        05 VARDE                  PIC S9(13)V9(2).                        
003700        05 IDSTATNR               PIC S9(9).                              
003800        05 VKARTTOT               PIC S9(17).                             
003900        05 KDARTURS               PIC X(2).                               
003910        05 IDVAT                  PIC X(17).                              
003920        05 KDINTTYP               PIC S9(2).                              
004000        05 FILLER                 PIC X(2).                               
004100*                                                                         
004200     03 FAKTURA-AREA              REDEFINES FIL-AREA.                     
004300*                                 FAKTURAUPPGIFTER POSTTYP 03             
004400        05 LANDKOD                PIC X(2).                               
004500        05 TRANSPORTSATT          PIC X(1).                               
004600*          ANGES ENDAST VID ÖVERENSKOMMELSE MED VTAB,                     
004700*          ANNARS BLANK                                                   
004800        05 LEVKUNDID              PIC X(9).                               
004900        05 LEVKUNDNAMN            PIC X(35).                              
005000        05 MOMSREGNR              PIC X(17).                              
005100        05 FAKTURANR              PIC X(12).                              
005200        05 FAKTURADATUM           PIC X(6).                               
005300        05 VARDE                  PIC S9(13)V9(2).                        
005400*                                                                         
005500     03 VARUPOST-AREA             REDEFINES FIL-AREA.                     
005600*                                 VARUPOSTUPPGIFTER POSTTYP 04            
005700        05 LANDKOD                PIC X(2).                               
005800        05 TRANSPORTSATT          PIC X(1).                               
005900        05 TRANSAKTIONSTYP        PIC X(1).                               
006000        05 STATNR                 PIC X(8).                               
006100        05 ANNAN-KVANTITET        PIC 9(8).                               
006200        05 VARDE                  PIC S9(11).                             
006300        05 NETTOVIKT              PIC 9(9).                               
006400        05 FILLER                 PIC X(57).                              
006500*                                                                         
006600*** END COPY A7290A01    LENGTH=99                                        
