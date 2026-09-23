000010*** EDIT ALLOWED                                                          
001410*             VISAR VILKA TABELLER SOM HAR SK. SAMLINGSNUMMER             
001420*             ALLTSÅ EN TABELL DÄR ALLA ARTIKLAR INOM TABELLEN            
001430*             LAGERSALDOBOKAS PÅ ETT ARTIKELNR(SAMLINGSNR)                
001440*             UPPFÖLJNING I BYTES SKER DOCK MOT URSP NUMMER               
001500*                                                                         
001600*                                                                         
001700*                                                                         
001800*                        OBS!    OCCURS MÅSTE VARA RÄTT !                 
001810 01  WWBYT17.                                                             
001900    03  TABELL1.                                                          
002100        05  FILLER              PIC 9(13) VALUE  2300009037316.           
002110        05  FILLER              PIC 9(13) VALUE  2400009037303.           
002120        05  FILLER              PIC 9(13) VALUE  2900009037171.           
002130        05  FILLER              PIC 9(13) VALUE  2950009037437.           
002200        05  FILLER              PIC 9(13) VALUE  4300005009971.           
002400        05  FILLER              PIC 9(13) VALUE  4350005009918.           
002500        05  FILLER              PIC 9(13) VALUE  4550005009847.           
003200    03  SAMLINGSTABELL REDEFINES TABELL1                                  
003300                             OCCURS 7                                     
003400                             ASCENDING TAB1-KVITT                         
003500                      INDEXED BY TAB1RAD.                                 
003600          05  TAB1-KVITT-TAB.                                             
005100              07  TAB1-KVITT      PIC 9(3).                               
005200              07  FILLER          PIC 9.                                  
005300          05  TAB1-IDARTNR-OBJ        PIC 9(9).                           
005400*** END COPY WWBYT01CC0  LENGTH=171   OLD LENGTH=0                        
