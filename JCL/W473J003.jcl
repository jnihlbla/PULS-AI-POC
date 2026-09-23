//W473J003 JOB (670W4730100W473J003,W100),'RTN W473S1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
//*+JBS BIND IMG0                                                               
/*JOBPARM TIME=1,LINES=5,CARDS=0,FORMS=1800                                     
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//        EXEC W473P003                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W473J003                                         
