//W221J02A JOB (640W2210100W221J02A,W100),'RTN W221S3',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM LINES=999,FORMS=1800,LINECT=0                                         
//PROC JCLLIB ORDER=(W.QASE.PROCLIB)                                            
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*AVROP01(&AVROP01)                                                            
//*AVROP02(&AVROP02)                                                            
//*AVROP03(&AVROP03)                                                            
//*AVROP04(&AVROP04)                                                            
//*AVROP05(&AVROP05)                                                            
//*AVROP06(&AVROP06)                                                            
//*AVROP07(&AVROP07)                                                            
//*AVROP08(&AVROP08)                                                            
//*AVROP09(&AVROP09)                                                            
//*AVROP10(&AVROP10)                                                            
//*                                                                             
//W221     EXEC W221P02A                                                        
//*                                                                             
//* PARAMETER FILE FROM SCREEN 2149                                             
//W2212A.W2212AD1 DD *                                                          
&AVROP01.                                                                       
&AVROP02.                                                                       
&AVROP03.                                                                       
&AVROP04.                                                                       
&AVROP05.                                                                       
&AVROP06.                                                                       
&AVROP07.                                                                       
&AVROP08.                                                                       
&AVROP09.                                                                       
&AVROP10.                                                                       
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W221J02A                                         
