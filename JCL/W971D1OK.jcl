//W971D1OK JOB (540W0000100W971D1OK,W100),'RTN W971D1',                         
//         CLASS=K                                                              
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM LINECT=0,FORMS=1800,LINES=50                                          
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
/*AFTER MEMOAPIX                                                                
//*---  WMEMOSND,EXC                                                            
//*                                                                             
//*  **************************************************************             
//*  *                                                            *             
//*  *  FILEMON-ÖVERFÖRINGEN TILL RS HAR GÅTT BRA!                *             
//*  *  DETTA JOBB GÖR ORDER PÅ W971D1                            *             
//*  *                                                            *             
//*  **************************************************************             
//*                                                                             
//OWDD1  EXEC WSOP,COMMAND='ORDER W971D1'                                       
