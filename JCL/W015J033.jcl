//W015J033 JOB (670W0150100W015J033,W100),'RTN W015XX',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//* OM BMP:N SKA HA KONTROLLKORT ÄNDRA TILL RÄTT NAMN NEDAN                     
//* ANNARS TA BORT KORTET                                                       
/*CNTL XXXXXXXX,EXC                                                             
//*                                                                             
//W015    EXEC W015P033,                                                        
//             DSIN=W015.W015XX.W01533(+0)                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W015J033                                         
