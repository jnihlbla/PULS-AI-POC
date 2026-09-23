000100 01  W2711B.                                                              
000200*                                 EXTRACT FILE WHEN KDREFORS=P            
000300     03 FLFLYG               PIC X.                                       
000400*                                 FLYGARTIKEL                             
000500     03 FLPB-JUST            PIC X.                                       
000600*                                 PERIODBEHOVSJUSTERING FLAGGA            
000700     03 FLREFBEO             PIC X.                                       
000800*                                 AUTOMATISK REFILL BEORDRING?            
000900     03 FLSEASON             PIC X.                                       
001000*                                 SÄSONG PÅ ARTIKEL                       
001100     03 IDARTNR              PIC S9(9)           COMP-3.                  
001200*                                 ARTIKELNUMMER                           
001300     03 IDDC                 PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 IDDC-REF             PIC X(2).                                    
001600*                                 SÄNDANDE LAGER FÖR REFILL               
001700     03 IDPERSON-BUY         PIC S9(3)           COMP-3.                  
001800*                                 PERSONKOD REFILLANSVARIG                
001900     03 KDERS                PIC S9(3)           COMP-3.                  
002000*                                 ERSÄTTNINGSKOD                          
002100     03 KDPRODSL             PIC S9(3)           COMP-3.                  
002200*                                 PRODUKTSLAG                             
002300     03 KDREFTYP             PIC X.                                       
002400*                                 TYP AV REFILLORDER                      
002500     03 KVAKS-PAV            PIC S9(7)           COMP-3.                  
002600*                                 DEL AV AK PÅ VÄG                        
002700     03 KVAKS-SDC            PIC S9(7)           COMP-3.                  
002800*                                 DEL AV AK SOM LIGGER I SDC              
002900     03 KVBEART-PROP         PIC S9(7)           COMP-3.                  
003000*                                 BESTÄLLT ANTAL STYCKEN                  
003100     03 KVBEART              PIC S9(7)           COMP-3.                  
003200*                                 BESTÄLLT ANTAL STYCKEN                  
003300     03 KVDISP               PIC S9(7)           COMP-3.                  
003400*                                 DISPONIBELT LAGER                       
003500     03 KVPB-REF             PIC S9(6)V9(1)      COMP-3.                  
003600*                                 PERIODBEHOV REFILLING                   
003700     03 KVPBREOI             PIC S9(6)V9(1)      COMP-3.                  
003800*                                 PERIODBEHOV FÖR REFILL OI               
003900     03 KVREFBER             PIC S9(7)           COMP-3.                  
004000*                                 BERÄKNAD REFILLINGKVANTITET             
004100     03 KVREFPKT             PIC S9(7)           COMP-3.                  
004200*                                 BERÄKNAD PÅFYLLNADSPUNKT                
004300     03 KVROS                PIC S9(7)           COMP-3.                  
004400*                                 RESTORDERSALDO                          
004500     03 PRMATRL              PIC S9(7)V9(2)      COMP-3.                  
004600*                                 FAST PRIS UNDER LÖPANDE ÅR              
004700     03 TEREFTXT             PIC X(10).                                   
004800*                                 REFILL VARNINGSTEXT                     
004900     03 TIORDREG             PIC S9(7)           COMP-3.                  
005000*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
005100     03 TIREFEFT             PIC S9(7)           COMP-3.                  
005200*                                 DATUM SENAST EFTERFRÅGAD                
005300*** END OF VILMAII-COPY LENGTH= 83 BYTES                                  
